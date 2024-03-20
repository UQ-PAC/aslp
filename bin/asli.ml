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

let opt_prelude : string ref = ref "prelude.asl"
let opt_filenames : string list ref = ref []
let opt_print_version = ref false
let opt_no_default_aarch64 = ref false
let opt_print_aarch64_dir = ref false
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
    {|:ast <instr-set> <int> [file]  Decode and write opcode semantics to stdout or a file, in a structured ast format|};
    {|:gen <instr-set> <regex>       Generate an offline lifter using the given backend|};
    {|      [backend] [dir]|};
    {|:project <file>                Execute ASLi commands in <file>|};
    {|:q :quit                       Exit the interpreter|};
    {|:run                           Execute instructions|};
    {|:set impdef <string> = <expr>  Define implementation defined behavior|};
    {|:set +<flag>                   Set flag|};
    {|:set -<flag>                   Clear flag|};
    {|:init globals                  Initializes global variables to concrete values (for evaluation)|};
    {|:init regs                     Initializes registers to concrete values (for evaluation)|};
    {|:coverage <instr-set> <regex>  Runs differential testing of partial and concrete evaluation|};
    {|<expr>                         Execute ASL expression|};
    {|<stmt> ;                       Execute ASL statement|}
]

(** supported backends for :gen and their default output directories *)
let gen_backends = [
    ("ocaml", (Cpu.Ocaml, "offlineASL"));
    ("cpp",   (Cpu.Cpp, "offlineASL-cpp"));
]

let flags = [
    ("trace:write", Eval.trace_write);
    ("trace:fun",   Eval.trace_funcall);
    ("trace:prim",  Eval.trace_primop);
    ("trace:instr", Eval.trace_instruction);
    ("eval:concrete_unknown", Value.concrete_unknown)
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
                    flush stdout;
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
                let lenv = Dis.build_env disEnv in

                let opcode = input_line inchan in
                let op = Value.VBits (Primops.prim_cvt_int_bits (Z.of_int 32) (Z.of_int (int_of_string opcode))) in

                (* Printf.printf "PRE Eval env: %s\n\n" (Testing.pp_eval_env evalEnv);
                Printf.printf "PRE Dis eval env: %s\n\n" (Testing.pp_eval_env disEvalEnv); *)

                (try
                    (* Evaluate original instruction *)
                    Eval.eval_decode_case AST.Unknown evalEnv decoder op;

                    (try
                        (* Generate and evaluate partially evaluated instruction *)
                        let disStmts = Dis.dis_decode_entry disEnv lenv decoder op in
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
        let op = Z.of_string opcode in
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
        let cpu' = Cpu.mkCPU cpu.env cpu.denv in
        let op = Z.of_string opcode in
        Printf.printf "Decoding instruction %s %s\n" iset (Z.format "%x" op);
        cpu'.sem iset op
    | ":ast" :: iset :: opcode :: rest when List.length rest <= 1 ->
        let op = Value.VBits (Primops.prim_cvt_int_bits (Z.of_int 32) (Z.of_string opcode)) in
        let decoder = Eval.Env.getDecoder cpu.env (Ident iset) in
        let chan_opt = Option.map open_out (List.nth_opt rest 0) in
        let chan = Option.value chan_opt ~default:stdout in
        List.iter
            (fun s -> Printf.fprintf chan "%s\n" (Utils.to_string (PP.pp_raw_stmt s)))
            (Dis.dis_decode_entry cpu.env cpu.denv decoder op);
        Option.iter close_out chan_opt
    | ":gen" :: iset :: id :: rest when List.length rest <= 2 ->
        let backend = Option.value List.(nth_opt rest 0) ~default:"ocaml" in
        Printf.printf "Generating lifter for %s %s using %s backend\n" iset id backend;

        let (backend, default_dir) = match List.assoc_opt backend gen_backends with
            | Some x -> x
            | None -> invalid_arg @@ Printf.sprintf "unknown backend %s (supported: %s)"
                                     backend (String.concat ", " List.(map fst gen_backends)) in

        let dir = Option.value List.(nth_opt rest 1) ~default:default_dir in
        let cpu' = Cpu.mkCPU cpu.env cpu.denv in
        cpu'.gen iset id backend dir
    | ":dump" :: iset :: opcode :: rest ->
        let fname = 
            (match rest with 
            | [] -> "sem.aslb"
            | [x] -> x 
            | _ -> invalid_arg "expected at most 3 arguments to :dump")
        in
        let cpu' = Cpu.mkCPU (Eval.Env.copy cpu.env) cpu.denv in
        let op = Z.of_string opcode in
        let bits = VBits (Primops.prim_cvt_int_bits (Z.of_int 32) op) in
        let decoder = Eval.Env.getDecoder cpu'.env (Ident iset) in
        let stmts = Dis.dis_decode_entry cpu'.env cpu.denv decoder bits in
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
    ( "--no-aarch64", Arg.Set opt_no_default_aarch64 , "       Disable bundled AArch64 semantics");
    ( "--aarch64-dir", Arg.Set opt_print_aarch64_dir, "       Print directory of bundled AArch64 semantics");
    ( "--version", Arg.Set opt_print_version, "       Print version");
    ( "--prelude", Arg.Set_string opt_prelude,"       ASL prelude file (default: ./prelude.asl)");
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
    else if !opt_print_aarch64_dir then 
        match aarch64_asl_dir with 
        | Some d -> Printf.printf "%s\n" d
        | None -> (Printf.eprintf "Unable to retrieve installed asl directory\n"; exit 1)
    else begin
        if !opt_verbose then List.iter print_endline banner;
        if !opt_verbose then print_endline "\nType :? for help";
        let env_opt =
            if (!opt_no_default_aarch64)  
            then evaluation_environment !opt_prelude !opt_filenames !opt_verbose
            else begin
                if List.length (!opt_filenames) != 0 then
                    Printf.printf
                        "Warning: asl file arguments ignored without --no-aarch64 (%s)\n"
                        (String.concat " " !opt_filenames)
                else ();
                aarch64_evaluation_environment ~verbose:!opt_verbose ();
            end in
        let env = (match env_opt with 
            | Some e -> e
            | None -> failwith "Unable to build evaluation environment.") in
        if not !opt_no_default_aarch64 then
            opt_filenames := snd (Option.get aarch64_asl_files); (* (!) should be safe if environment built successfully. *)
        if !opt_verbose then Printf.printf "Built evaluation environment\n";
        Dis.debug_level := !opt_debug_level;

        LNoise.history_load ~filename:"asl_history" |> ignore;
        LNoise.history_set ~max_length:100 |> ignore;
        
        let denv = Dis.build_env env in
        let prj_files = List.filter (fun f -> Utils.endswith f ".prj") !opt_filenames in
        let tcenv = TC.Env.mkEnv TC.env0 and cpu = Cpu.mkCPU env denv in
        List.iter (fun f -> process_command tcenv cpu "<args>" (":project " ^ f)) prj_files;
        repl tcenv cpu
    end

let _ = ignore (main ())

(****************************************************************
 * End
 ****************************************************************)
