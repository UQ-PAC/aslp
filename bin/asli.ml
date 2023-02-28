(****************************************************************
 * ASL interactive frontend
 *
 * Copyright Arm Limited (c) 2017-2019
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL interactive frontend *)

open LibASL

open Asl_ast
open Value
open Eval
open Asl_utils

module Parser = Asl_parser
module TC     = Tcheck
module PP     = Asl_parser_pp
module AST    = Asl_ast

let opt_filenames : string list ref = ref []
let opt_print_version = ref false
let opt_verbose = ref false

let opt_debug_level = ref 0

let () = Printexc.register_printer
    (function
    | Value.EvalError (loc, msg) ->
        Some (Printf.sprintf "EvalError at %s: %s" (pp_loc loc) msg)
    | _ -> None)

let help_msg = [
    {|:? :help                       Show this help message|};
    {|:elf <file>                    Load an ELF file|};
    {|:opcode <instr-set> <int>      Decode and execute opcode|};
    {|:sem <instr-set> <int>         Decode and print opcode semantics|};
    {|:project <file>                Execute ASLi commands in <file>|};
    {|:q :quit                       Exit the interpreter|};
    {|:run                           Execute instructions|};
    {|:set impdef <string> = <expr>  Define implementation defined behavior|};
    {|:set +<flag>                   Set flag|};
    {|:set -<flag>                   Clear flag|};
    {|<expr>                         Execute ASL expression|};
    {|<stmt> ;                       Execute ASL statement|}
]

let flags = [
    ("trace:write", Eval.trace_write);
    ("trace:fun",   Eval.trace_funcall);
    ("trace:prim",  Eval.trace_primop);
    ("trace:instr", Eval.trace_instruction)
]

let mkLoc (fname: string) (input: string): AST.l =
    let len = String.length input in
    let start : Lexing.position = { pos_fname = fname; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 } in
    let finish: Lexing.position = { pos_fname = fname; pos_lnum = 1; pos_bol = 0; pos_cnum = len } in
    AST.Range (start, finish)

let () = Random.self_init ()

let rec process_command (tcenv: TC.Env.t) (cpu: Cpu.cpu) (fname: string) (input0: string): unit =
    let input = String.trim input0 in
    (match String.split_on_char ' ' input with
    | [""] ->
        ()
    | [":init"; "globals"] ->
        Eval.initializeGlobals cpu.env;
    | [":init"; "regs"] ->
        let vals = (List.init 64 (fun _ -> Z.of_int64 (Random.int64 Int64.max_int))) in
        Eval.initializeRegistersAndMemory cpu.env vals;
    | ":enumerate" :: iset :: tail ->
        let (start,stop,fname) = (match tail with
        | [start;stop;fname] -> (int_of_string start, int_of_string stop, fname)
        | [] -> (0, Int.shift_left 1 32, "ops.txt")
        | _ -> invalid_arg "invalid argument to :enumerate") in

        let decoder = Eval.Env.getDecoder cpu.env (Ident iset) in
        Testing.enumerate_opcodes cpu.env decoder start stop fname
    | [":coverage"; iset; instr] ->
        let open Testing in
        let encodings = get_opcodes opt_verbose iset instr cpu.env in
        List.iter (fun (enc, fields, opt_opcodes) ->
            Printf.printf "\nENCODING: %s\n" enc;
            match opt_opcodes with
            | None -> Printf.printf "(encoding unused)\n";
            | Some opcodes ->
                List.iter (fun (op, valid) ->
                    let fs = fields_of_opcode fields op in
                    Printf.printf "%s: %s --> " (hex_of_int op) (pp_enc_fields fs);
                    if valid then
                        let result = op_test_opcode cpu.env iset op in
                        Printf.printf "%s\n" (pp_opresult (fun _ -> "OK") result)
                    else Printf.printf "(invalid)\n";
                ) opcodes
        ) encodings;
    | [":compare"; iset; file] ->
        let decoder = Eval.Env.getDecoder cpu.env (Ident iset) in
        let inchan = open_in file in
        (try
            while true do
                (* Set up our environments *)
                let initEnv = Eval.Env.copy cpu.env in
                (* Obtain and set random initial values for _R registers. *)
                let vals = (List.init 64 (fun _ -> Z.of_int64 (Random.int64 Int64.max_int))) in
                Eval.initializeRegistersAndMemory initEnv vals;
                (* Replace remaining VUninitialized with default zero values. *)
                Eval.initializeGlobals initEnv;

                (* Disassembly uses original uninitialised environment.
                   Others use the randomly initialised environment for full evaluation. *)
                let disEnv = Eval.Env.copy cpu.env in
                let evalEnv = Eval.Env.copy initEnv in
                let disEvalEnv = Eval.Env.copy initEnv in

                let opcode = input_line inchan in
                let op = Value.VBits (Primops.prim_cvt_int_bits (Z.of_int 32) (Z.of_int (int_of_string opcode))) in

                (* Printf.printf "PRE Eval env: %s\n\n" (Testing.pp_eval_env evalEnv);
                Printf.printf "PRE Dis eval env: %s\n\n" (Testing.pp_eval_env disEvalEnv); *)

                (try
                    (* Evaluate original instruction *)
                    Eval.eval_decode_case AST.Unknown evalEnv decoder op;

                    (try
                        (* Generate and evaluate partially evaluated instruction *)
                        let disStmts = Dis.dis_decode_entry disEnv decoder op in
                        List.iter (eval_stmt disEvalEnv) disStmts;

                        if Eval.Env.compare evalEnv disEvalEnv then
                            Printf.printf "No errors detected\n"
                        else
                            Printf.printf "Environments not equal\n";
                    with
                        EvalError (loc, message) -> Printf.printf "Dis error: %s\n" message
                    )
                with
                    EvalError (loc, message) -> Printf.printf "Eval error: %s\n" message;
                );
                (* Printf.printf "POST Eval env: %s\n\n" (Testing.pp_eval_env evalEnv);
                Printf.printf "POST Dis eval env: %s\n\n" (Testing.pp_eval_env disEvalEnv); *)
            done
        with
        | End_of_file ->
            close_in inchan
        )
    | [":elf"; file] ->
        Printf.printf "Loading ELF file %s.\n" file;
        let entry = Elf.load_file file cpu.elfwrite in
        Printf.printf "Entry point = 0x%Lx\n" entry
    | [":help"] | [":?"] ->
        List.iter print_endline help_msg;
        print_endline "\nFlags:";
        List.iter (fun (nm, v) -> Printf.printf "  %s%s\n" (if !v then "+" else "-") nm) flags
    | [":opcode"; iset; opcode] ->
        (* todo: make this code more robust *)
        let op = Z.of_int (int_of_string opcode) in
        Printf.printf "Decoding and executing instruction %s %s\n" iset (Z.format "%x" op);
        cpu.opcode iset op
    | [":opcodes"; iset; instr] ->
        let open Testing in
        let encodings = get_opcodes opt_verbose iset instr cpu.env in
        List.iter (fun (_, _, opt_opcodes) ->
            match opt_opcodes with
            | None -> ()
            | Some opcodes ->
                List.iter (fun (op, valid) ->
                    if valid then
                        let op' = (String.sub (hex_of_int op) 2 8) in
                        Printf.printf "%c" (String.get op' 6);
                        Printf.printf "%c " (String.get op' 7);
                        Printf.printf "%c" (String.get op' 4);
                        Printf.printf "%c " (String.get op' 5);
                        Printf.printf "%c" (String.get op' 2);
                        Printf.printf "%c " (String.get op' 3);
                        Printf.printf "%c" (String.get op' 0);
                        Printf.printf "%c " (String.get op' 1);
                ) opcodes
        ) encodings;
    | [":sem"; iset; opcode] ->
        let cpu' = Cpu.mkCPU (Eval.Env.copy cpu.env) in
        let op = Z.of_int (int_of_string opcode) in
        Printf.printf "Decoding instruction %s %s\n" iset (Z.format "%x" op);
        cpu'.sem iset op
    | ":dump" :: iset :: opcode :: rest ->
        let fname = 
            (match rest with 
            | [] -> "sem.aslb"
            | [x] -> x 
            | _ -> invalid_arg "expected at most 3 arguments to :dump")
        in
        let cpu' = Cpu.mkCPU (Eval.Env.copy cpu.env) in
        let op = Z.of_int (int_of_string opcode) in
        let bits = VBits (Primops.prim_cvt_int_bits (Z.of_int 32) op) in
        let decoder = Eval.Env.getDecoder cpu'.env (Ident iset) in
        let stmts = Dis.dis_decode_entry cpu'.env decoder bits in
        let chan = open_out_bin fname in
        Printf.printf "Dumping instruction semantics for %s %s" iset (Z.format "%x" op);
        Printf.printf " to file %s\n" fname;
        (* NOTE: .aslb file is a marshalled `stmt list` *)
        Marshal.to_channel chan (stmts : stmt list) [];
        close_out chan
    | (":set" :: "impdef" :: rest) ->
        let cmd = String.concat " " rest in
        let loc = mkLoc fname cmd in
        let (x, e) = LoadASL.read_impdef tcenv loc cmd in
        let v = Eval.eval_expr loc cpu.env e in
        Eval.Env.setImpdef cpu.env x v
    | [":set"; flag] when Utils.startswith flag "+" ->
        (match List.assoc_opt (Utils.stringDrop 1 flag) flags with
        | None -> Printf.printf "Unknown flag %s\n" flag;
        | Some f -> f := true
        )
    | [":set"; flag] when Utils.startswith flag "-" ->
        (match List.assoc_opt (Utils.stringDrop 1 flag) flags with
        | None -> Printf.printf "Unknown flag %s\n" flag;
        | Some f -> f := false
        )
    | [":project"; prj] ->
        let inchan = open_in prj in
        (try
            while true do
                process_command tcenv cpu prj (input_line inchan)
            done
        with
        | End_of_file ->
            close_in inchan
        )
    | [":q"] | [":quit"] ->
        exit 0
    | [":run"] ->
        (try
            while true do
                cpu.step ()
            done
        with
        | Value.Throw (_, Primops.Exc_ExceptionTaken) ->
            Printf.printf "Exception taken\n"
        )
    | _ ->
        if ';' = String.get input (String.length input - 1) then begin
            let s = LoadASL.read_stmt tcenv input in
            Eval.eval_stmt cpu.env s
        end else begin
            let loc = mkLoc fname input in
            let e   = LoadASL.read_expr tcenv loc input in
            let v   = Eval.eval_expr loc cpu.env e in
            print_endline (Value.pp_value v)
        end
    )

let rec repl (tcenv: TC.Env.t) (cpu: Cpu.cpu): unit =
    flush stdout;
    (match LNoise.linenoise "ASLi> " with
    | None -> ()
    | Some input ->
        LNoise.history_add input |> ignore;
        (try
            LoadASL.report_eval_error (fun _ -> ()) (fun _ ->
                LoadASL.report_type_error (fun _ -> ()) (fun _ ->
                    LoadASL.report_parse_error (fun _ -> ()) (fun _ ->
                        process_command tcenv cpu "<stdin>" input
                    )
                )
            )
        with
        | exc ->
            Printf.printf "  Error %s\n" (Printexc.to_string exc);
            Printexc.print_backtrace stdout;
            (* truncate backtrace to 1000 characters. *)
            (* let bt = Printexc.get_backtrace () in
            let k = if 1000 < String.length bt then String.index_from bt 1000 '\n' else String.length bt in
            Printf.printf "%s\n[...]\n" (String.sub bt 0 k) *)
        );
        repl tcenv cpu
    )

let options = Arg.align ([
    ( "-x", Arg.Set_int opt_debug_level,      "       Debugging output");
    ( "-v", Arg.Set opt_verbose,              "       Verbose output");
    ( "--version", Arg.Set opt_print_version, "       Print version");
] )

let version = "ASL 0.2.0 alpha"

let banner = [
    {|            _____  _       _    ___________________________________|};
    {|    /\     / ____|| |     (_)   ASL interpreter                    |};
    {|   /  \   | (___  | |      _    Copyright Arm Limited (c) 2017-2019|};
    {|  / /\ \   \___ \ | |     | |                                      |};
    {| / ____ \  ____) || |____ | |   |} ^ version;
    {|/_/    \_\|_____/ |______||_|   ___________________________________|}
]
let usage_msg =
    ( version
    ^ "\nusage: asl <options> <file1> ... <fileN>\n"
    )

let _ =
  Arg.parse options
    (fun s -> opt_filenames := (!opt_filenames) @ [s])
    usage_msg

let main () =
    if !opt_print_version then Printf.printf "%s\n" version
    else begin
        if !opt_verbose then List.iter print_endline banner;
        if !opt_verbose then print_endline "\nType :? for help";
        let t  = LoadASL.read_file "prelude.asl" true !opt_verbose in
        let ts = List.map (fun filename ->
            if Utils.endswith filename ".spec" then begin
                LoadASL.read_spec filename !opt_verbose
            end else if Utils.endswith filename ".asl" then begin
                LoadASL.read_file filename false !opt_verbose
            end else if Utils.endswith filename ".prj" then begin
                [] (* ignore project files here and process later *)
            end else begin
                failwith ("Unrecognized file suffix on "^filename)
            end
        ) !opt_filenames
        in

        if !opt_verbose then Printf.printf "Building evaluation environment\n";
        let env = (try
            Eval.build_evaluation_environment (List.concat (t::ts))
        with
        | Value.EvalError (loc, msg) ->
            Printf.printf "  %s: Evaluation error: %s\n" (pp_loc loc) msg;
            exit 1
        ) in
        if !opt_verbose then Printf.printf "Built evaluation environment\n";
        Dis.debug_level := !opt_debug_level;

        LNoise.history_load ~filename:"asl_history" |> ignore;
        LNoise.history_set ~max_length:100 |> ignore;
        
        let prj_files = List.filter (fun f -> Utils.endswith f ".prj") !opt_filenames in
        let tcenv = TC.Env.mkEnv TC.env0 and cpu = Cpu.mkCPU env in
        List.iter (fun f -> process_command tcenv cpu "<args>" (":project " ^ f)) prj_files;
        repl tcenv cpu
    end

let _ =ignore(main ())

(****************************************************************
 * End
 ****************************************************************)
