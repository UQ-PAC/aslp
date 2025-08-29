module AST = Asl_ast
module Env = Eval.Env

open AST
open Value
open Asl_utils

(****************************************************************
 * Opcode decoding without evaluation.
 ****************************************************************)

(* Copies of eval_decode_alt and eval_decode_case which do not evaluate their opcodes.
   These "try" decoding each opcode. *)

(** Try to evaluate an "encoding" block to the given opcode. *)
let rec try_encoding (env: Env.t) (x: encoding) (op: Primops.bigint): bool =
  let Encoding_Block (nm, iset, fields, opcode, guard, unpreds, b, loc) = x in
  (* todo: consider checking iset *)
  (* Printf.printf "Checking opcode match %s == %s\n" (Utils.to_string (PP.pp_opcode_value opcode)) (pp_value op); *)
  let trace_instruction = ref false in
  match Eval.eval_opcode_guard loc opcode op with
  | Some op ->
      if !trace_instruction then Printf.printf "TRACE: instruction %s\n" (pprint_ident nm);
      List.iter (function (IField_Field (f, lo, wd)) ->
          let v = extract_bits' loc op lo wd in
          if !trace_instruction then Printf.printf "      %s = %s\n" (pprint_ident f) (pp_value v);
          Env.addLocalVar loc env f v
      ) fields;
      if to_bool loc (Eval.eval_expr loc env guard) then begin
          List.iter (fun (i, b) ->
              if eval_eq loc (extract_bits' loc op i 1) (from_bitsLit b) then
                  raise (Throw (loc, Exc_Unpredictable))
          ) unpreds;
          (* try_encoding: do NOT evaluate __decode block because this may
             require IMPDEF values.
             assume its conditions are verified by earlier decode case matches. *)
          (* List.iter (eval_stmt env) b; *)
          true
      end else begin
          false
      end
  | None -> false

(** Tests whether the given opcode can be decoded by the given decode case. *)
and try_decode_case (loc: AST.l) (env: Env.t) (x: decode_case) (op: Primops.bigint): ident option =
    (match x with
    | DecoderCase_Case (ss, alts, loc) ->
            let vs = List.map (fun s -> Eval.eval_decode_slice loc env s op) ss in
            let rec eval alts =
                (match alts with
                | (alt :: alts') ->
                        (match try_decode_alt loc env alt vs op with
                        | Some e -> Some e
                        | None -> eval alts')
                | [] ->
                        None (*raise (EvalError (loc, "unmatched decode pattern"))*)
                )
            in
            eval alts
    )

(** Tests whether the given opcode is decodable by the given decode case alternative.  *)
and try_decode_alt (loc: AST.l) (env: Env.t) (DecoderAlt_Alt (ps, b)) (vs: value list) (op: Primops.bigint): ident option =
    if List.for_all2 (Eval.eval_decode_pattern loc) ps vs then
        (match b with
        | DecoderBody_UNPRED loc -> raise (Throw (loc, Exc_Unpredictable))
        | DecoderBody_UNALLOC loc -> raise (Throw (loc, Exc_Undefined))
        | DecoderBody_NOP loc -> None (* Some (Ident "__NOP") *)
        | DecoderBody_Encoding (encname, l) ->
                let (enc, opost, cond, exec) = Env.getInstruction loc env encname in
                if try_encoding env enc op then begin
                    Some encname
                end else begin
                    None
                end
        | DecoderBody_Decoder (fs, c, loc) ->
                (* let env = Env.empty in  *)
                List.iter (function (IField_Field (f, lo, wd)) ->
                    let op = Value.from_bitsInt (lo+wd) op in
                    Env.addLocalVar loc env f (extract_bits' loc op lo wd)
                ) fs;
                try_decode_case loc env c op
        )
    else
        None

(****************************************************************
 * Data type for storing intervals of integers in a compressed format.
 ****************************************************************)

(** A list of intervals, where the pair (lo,hi) indicates that all
    integers x such that lo <= x <= hi are in the list. *)
type pair_list = (int * int) list

let pair_list_cons i: pair_list -> pair_list =
  function
  | (l,r)::rest when i = succ r -> (l,i)::rest
  | rest -> (i,i)::rest

let pp_pair_list =
  Utils.pp_list (fun (x,y) -> string_of_int x ^ "," ^ string_of_int y)

let rec pair_list_mem (xs: pair_list) (x: int): bool =
  match xs with
  | [] -> false
  | (l,r)::rest ->
    if l <= x && x <= r then
      true
    else
      pair_list_mem rest x

type pair_array = (int * int) array

(** Searches for value in given interval array.
    Assumes array's intervals are in *increasing* order.  *)
let pair_array_mem (xs: pair_array) (x: int): bool =
  let l = ref 0
  and r = ref (Array.length xs - 1) in
  while !l < !r do
    let m = (!l + !r) / 2 in
    if x > snd (Array.get xs m) then
      l := m + 1
    else
      r := m
  done;
  let (lo,hi) = Array.get xs !l in
  lo <= x && x <= hi

  (****************************************************************
 * Opcode enumeration functions.
 ****************************************************************)

let enumerate_opcodes (env: Env.t) (case: decode_case) start stop fname: unit =

  let debug_interval = Int.shift_left 1 20 in
  let i = ref start in
  let j = ref 1 in

  let t0 = (Sys.time ()) in
  let nprev = ref 0 in
  let tprev = ref t0 in
  let yes = ref 0 in
  let no = ref 0 in

  let result = ref Bindings.empty in

  let f = open_out fname in

  while !i <> stop do
    let opresult =
      (try try_decode_case Unknown env case (Z.of_int !i)
      with Throw _ -> None) in

    (match opresult with
    | Some e ->
        result := Bindings.update e
        (function
        | None -> Some [(!i,!i)]
        | Some old -> Some (pair_list_cons !i old)) !result;
        yes := succ !yes;
    | None ->
        no := succ !no);

    if (!j = debug_interval) then begin
      let n = !yes + !no in
      let t = Sys.time () -. t0 in

      let dn = n - !nprev in
      let dt = t -. !tprev in

      Printf.printf "t: %f, 0x%08x (%d): average %f/s (+%d in %f, %f/s), valid: %d, invalid: %d\n"
        t
        !i !i (float n /. t)
        dn dt (float dn /. dt)
        !yes !no;
      Stdlib.flush stdout;

      Printf.fprintf f "0x%08x (%d): %s\n" !i !i (pp_bindings (pp_pair_list) !result);
      Stdlib.flush f;
      result := Bindings.empty;

      nprev := n;
      tprev := t;
      j := 0;
    end;
    j := succ !j;
    i := succ !i;
  done

(* load opcodes interval file stored as lines of pairs,
   where each line is the difference from the previous line.

   for example:
   0x1,0x2
   0x4,0x6
   0x5,0x7

   is stored as:
   0x1,0x2
   0x3,0x4
   0x1,0x1
   *)
let load_opcode_file (p: string): (int * int) array =
  let f = open_in p in
  let l = ref 0
  and r = ref 0 in
  let ops = ref [] in
  (try
    while true do
      let line = input_line f in
      match String.split_on_char ',' line with
      | [l';r' ] ->
        let l' = int_of_string l'
        and r' = int_of_string r' in
        l := !l + l';
        r := !r + r';
        ops := (!l, !r) :: !ops
      | _ -> assert false
    done
  with End_of_file -> ());
  close_in_noerr f;
  let a = Array.of_list !ops in
  Array.fast_sort compare a;
  a

let load_opcodes (directory: string): (int*int) array Bindings.t option =
  try
    let files = Array.to_list @@ Sys.readdir directory in
    Some (mk_bindings
      (List.map
        (fun f -> (Ident f, load_opcode_file (Filename.concat directory f)))
        files))
  with
    Sys_error _ -> None

(****************************************************************
 * Opcode coverage testing.
 ****************************************************************)

module IntMap = Map.Make(struct
  type t = int
  let compare = compare
end)

let pp_intmap (f: 'a -> string) (m: 'a IntMap.t): string =
  let pairs = List.map (fun (k,v) -> Printf.sprintf "%d: %s" k (f v)) (IntMap.bindings m) in
  "{ " ^ String.concat ", " pairs ^ " }"

let hex_of_int = Printf.sprintf "0x%08x"


(** A tree of possible opcodes for a given encoding. *)
type encoding_tree =

  (* A single opcode. *)
  | Op of int

  (* A field branching into different subtrees depending on the value this
     field takes. The map is keyed by this field's values and has values of subtrees. *)
  | Field of instr_field * encoding_tree IntMap.t

let rec pp_enc_tree =
  function
  | Op i -> hex_of_int i
  | Field (f, t) ->
    let f' = pp_instr_field f in
    let t' = pp_intmap pp_enc_tree t in
    "[\"" ^ f' ^ "\": " ^ t' ^ "]"

type fields = (instr_field * int) list

let rec list_of_enc_tree (t: encoding_tree): int list =
  match t with
  | Op x -> [x]
  | Field (f, t') ->
    List.concat
      (List.map (fun (_,v) ->
        list_of_enc_tree v)
        (IntMap.bindings t'))

let pp_enc_fields (f: fields): string =
  Utils.pp_list (fun (f,i) -> pp_instr_field f ^ "=" ^ string_of_int i) f


let pp_enc_list (encs: (fields * int) list): string =
  String.concat "\n"
    (List.map (Utils.pp_pair pp_enc_fields hex_of_int) encs)

(* Functions for manipulating opcodes as integers. *)

let fields_of_opcode (fields: instr_field list) (op: int): fields =
  List.map (fun f ->
    let IField_Field(_,lo,wd) = f in
    let mask = Int.shift_left 1 wd - 1 in
    (f, Int.logand mask (Int.shift_right_logical op lo))
  ) fields

let set_field (IField_Field(_,lo,wd): instr_field) (op: int) (v: int) =
  (* assert that value is within bounds for given width. *)
  assert (0 <= v);
  assert (v < Int.shift_left 1 wd);

  (* mask to zero bits for the field before applying given value. *)
  let ones_wd = Int.shift_left 1 wd - 1 in
  let mask = Int.lognot (Int.shift_left ones_wd lo) in

  Int.logor (Int.logand op mask) (Int.shift_left v lo)

let int_of_opcode: opcode_value -> int =
  function
  | Opcode_Bits bits ->
    int_of_string ("0b" ^ drop_chars bits ' ')
  | Opcode_Mask mask ->
    let v = String.map (function | 'x' -> '0' | c -> c) mask in
    int_of_string ("0b" ^ drop_chars v ' ')


(* Functions for enumerating encodings with encoding_tree. *)

let field_vals_flags_only (enc: encoding) (name: string) (wd: int): int list =
  let Encoding_Block (instr, _, _, _, _, _, _, _) = enc in
  let bound = Int.shift_left 1 wd in
  let ones = bound - 1 in
  match (instr, name) with
  | Ident "aarch64_branch_unconditional_eret", "Rn" -> [0b11111]
  | Ident "aarch64_branch_unconditional_register", "Rn" -> [0; 1; 0b11111]
  | _, "cond" -> [1]
  | _ when Utils.startswith name "R" && name <> "R" -> [0;1;ones]
  | _ when Utils.startswith name "X" && name <> "X" -> [0;1;ones]
  | _ when Utils.startswith name "imm" -> [0;1;ones]
  | _ when Utils.startswith name "uimm" -> [1]
  | _ when Utils.startswith name "scale" -> [0]
  | _, ("b40") -> [0;1;ones]

  (* RISCV common cases *)
  | _ when Utils.startswith name "rs" -> [0;1;2;ones]
  | _ when Utils.startswith name "rd" -> [0;1;2;ones]
  | _ when Utils.startswith name "vs" -> [0;1;2;ones]
  | _ when Utils.startswith name "vd" -> [0;1;2;ones]
  | _, "rm" -> [ 0; 1; 2; 3; 4; 7 ]

  (* Just the entire opcode as a field... *)
  | Ident "ILLEGAL_0", _ -> [0]
  | Ident "FENCE_RESERVED_0", "fm" -> [0]
  | Ident "FENCE_RESERVED_0", "pred" -> [0]
  | Ident "FENCE_RESERVED_0", "succ" -> [0]

  (* Auto-generated *)
  | Ident "AMO_0", "op" -> [ 1; 0; 4; 12; 8; 16; 20; 24; 28 ]
  | Ident "BTYPE_0", "op" -> [ 0; 1; 4; 5; 6; 7 ]
  | Ident "CSRImm_0", "op" -> [ 1; 2; 3 ]
  | Ident "CSRReg_0", "op" -> [ 1; 2; 3 ]
  | Ident "FVFMATYPE_0", "funct6" -> [ 40; 41; 42; 43; 44; 45; 46; 47 ]
  | Ident "FVFMTYPE_0", "funct6" -> [ 24; 25; 27; 28; 29; 31 ]
  | Ident "FVFTYPE_0", "funct6" -> [ 0; 2; 4; 6; 8; 9; 10; 14; 15; 32; 33; 36; 39 ]
  | Ident "FVVMATYPE_0", "funct6" -> [ 40; 41; 42; 43; 44; 45; 46; 47 ]
  | Ident "FVVMTYPE_0", "funct6" -> [ 24; 25; 27; 28 ]
  | Ident "FVVTYPE_0", "funct6" -> [ 0; 2; 4; 6; 8; 9; 10; 32; 36 ]
  | Ident "FWFTYPE_0", "funct6" -> [ 52; 54 ]
  | Ident "FWVFMATYPE_0", "funct6" -> [ 60; 61; 62; 63 ]
  | Ident "FWVFTYPE_0", "funct6" -> [ 48; 50; 56 ]
  | Ident "FWVTYPE_0", "funct6" -> [ 52; 54 ]
  | Ident "FWVVMATYPE_0", "funct6" -> [ 60; 61; 62; 63 ]
  | Ident "FWVVTYPE_0", "funct6" -> [ 48; 50; 56 ]
  | Ident "ITYPE_0", "op" -> [ 0; 2; 3; 7; 6; 4 ]
  | Ident "LOAD_0", "is_unsigned" -> [ 1; 0 ]
  | Ident "LOAD_0", "width" -> [ 0; 1; 2; 3 ]
  | Ident "MMTYPE_0", "funct6" -> [ 25; 29; 24; 27; 26; 30; 28; 31 ]
  | Ident "MUL_0", "mul_opXN" -> [ 0; 1; 2; 3 ]
  | Ident "MVVMATYPE_0", "funct6" -> [ 45; 47; 41; 43 ]
  | Ident "MVVTYPE_0", "funct6" -> [ 8; 9; 10; 11; 37; 39; 36; 38; 32; 33; 34; 35 ]
  | Ident "MVXMATYPE_0", "funct6" -> [ 45; 47; 41; 43 ]
  | Ident "MVXTYPE_0", "funct6" -> [ 8; 9; 10; 11; 14; 15; 37; 39; 36; 38; 32; 33; 34; 35 ]
  | Ident "NISTYPE_0", "funct6" -> [ 44; 45 ]
  | Ident "NITYPE_0", "funct6" -> [ 46; 47 ]
  | Ident "NVSTYPE_0", "funct6" -> [ 44; 45 ]
  | Ident "NVTYPE_0", "funct6" -> [ 46; 47 ]
  | Ident "NXSTYPE_0", "funct6" -> [ 44; 45 ]
  | Ident "NXTYPE_0", "funct6" -> [ 46; 47 ]
  | Ident "REM_0", "is_unsigned" -> [ 1; 0 ]
  | Ident "REMW_0", "is_unsigned" -> [ 1; 0 ]
  | Ident "RFVVTYPE_0", "funct6" -> [ 3; 1; 7; 5; 51; 49 ]
  | Ident "RIVVTYPE_0", "funct6" -> [ 48; 49 ]
  | Ident "RMVVTYPE_0", "funct6" -> [ 0; 1; 2; 3; 4; 5; 6; 7 ]
  | Ident "UTYPE_0", "op" -> [ 55; 23 ]
  | Ident "VAESDF_0", "funct6" -> [ 40; 41 ]
  | Ident "VAESDM_0", "funct6" -> [ 40; 41 ]
  | Ident "VAESEF_0", "funct6" -> [ 40; 41 ]
  | Ident "VAESEM_0", "funct6" -> [ 40; 41 ]
  | Ident "VEXT2TYPE_0", "funct6" -> [ 6; 7 ]
  | Ident "VEXT4TYPE_0", "funct6" -> [ 4; 5 ]
  | Ident "VEXT8TYPE_0", "funct6" -> [ 2; 3 ]
  | Ident "VFNUNARY0_0", "vfnunary0XN" -> [ 16; 17; 18; 19; 20; 21; 22; 23 ]
  | Ident "VFUNARY0_0", "vfunary0XN" -> [ 0; 1; 2; 3; 6; 7 ]
  | Ident "VFUNARY1_0", "vfunary1XN" -> [ 0; 4; 5; 16 ]
  | Ident "VFWUNARY0_0", "vfwunary0XN" -> [ 8; 9; 10; 11; 12; 14; 15 ]
  | Ident "VICMPTYPE_0", "funct6" -> [ 24; 25; 28; 29; 30; 31 ]
  | Ident "VIMCTYPE_0", "funct6" -> [ 17 ]
  | Ident "VIMSTYPE_0", "funct6" -> [ 16 ]
  | Ident "VIMTYPE_0", "funct6" -> [ 17 ]
  | Ident "VISG_0", "funct6" -> [ 14; 15; 12 ]
  | Ident "VITYPE_0", "funct6" -> [ 0; 3; 9; 10; 11; 32; 33; 37; 40; 41; 42; 43 ]
  | Ident "VLOXSEGTYPE_0", "width" -> [ 0; 5; 6; 7 ]
  | Ident "VLRETYPE_0", "width" -> [ 0; 5; 6; 7 ]
  | Ident "VLSEGFFTYPE_0", "width" -> [ 0; 5; 6; 7 ]
  | Ident "VLSEGTYPE_0", "width" -> [ 0; 5; 6; 7 ]
  | Ident "VLSSEGTYPE_0", "width" -> [ 0; 5; 6; 7 ]
  | Ident "VLUXSEGTYPE_0", "width" -> [ 0; 5; 6; 7 ]
  | Ident "VMTYPE_0", "op" -> [ 7; 39 ]
  | Ident "VSOXSEGTYPE_0", "width" -> [ 0; 5; 6; 7 ]
  | Ident "VSSEGTYPE_0", "width" -> [ 0; 5; 6; 7 ]
  | Ident "VSSSEGTYPE_0", "width" -> [ 0; 5; 6; 7 ]
  | Ident "VSUXSEGTYPE_0", "width" -> [ 0; 5; 6; 7 ]
  | Ident "VVCMPTYPE_0", "funct6" -> [ 24; 25; 26; 27; 28; 29 ]
  | Ident "VVMCTYPE_0", "funct6" -> [ 17; 19 ]
  | Ident "VVMSTYPE_0", "funct6" -> [ 16; 18 ]
  | Ident "VVMTYPE_0", "funct6" -> [ 17; 19 ]
  | Ident "VVTYPE_0", "funct6" -> [ 0; 2; 4; 5; 6; 7; 9; 10; 11; 12; 14; 32; 33; 34; 35; 37; 39; 40; 41; 42; 43 ]
  | Ident "VXCMPTYPE_0", "funct6" -> [ 24; 25; 26; 27; 28; 29; 30; 31 ]
  | Ident "VXMCTYPE_0", "funct6" -> [ 17; 19 ]
  | Ident "VXMSTYPE_0", "funct6" -> [ 16; 18 ]
  | Ident "VXMTYPE_0", "funct6" -> [ 17; 19 ]
  | Ident "VXSG_0", "funct6" -> [ 14; 15; 12 ]
  | Ident "VXTYPE_0", "funct6" -> [ 0; 2; 3; 4; 5; 6; 7; 9; 10; 11; 32; 33; 34; 35; 37; 39; 40; 41; 42; 43 ]
  | Ident "WMVVTYPE_0", "funct6" -> [ 60; 61; 63 ]
  | Ident "WMVXTYPE_0", "funct6" -> [ 60; 61; 62; 63 ]
  | Ident "WRS_0", "op" -> [ 29; 13 ]
  | Ident "WVTYPE_0", "funct6" -> [ 53; 55; 52; 54 ]
  | Ident "WVVTYPE_0", "funct6" -> [ 49; 51; 48; 50; 59; 56; 58 ]
  | Ident "WVXTYPE_0", "funct6" -> [ 49; 51; 48; 50; 59; 56; 58 ]
  | Ident "WXTYPE_0", "funct6" -> [ 53; 55; 52; 54 ]
  | Ident "ZICBOM_0", "cbop" -> [ 1; 2; 0 ]
  | Ident "ZVKSHA2TYPE_0", "funct6" -> [ 46; 47 ]

  | _ -> List.init (min 256 bound) (fun x -> x)

let enumerate_encoding (enc: encoding) (field_vals: string -> int -> int list): encoding_tree =
  let Encoding_Block(name, iset, fields, opcode, guard, unpreds, stmts, loc) = enc in
  let rec go op fs =
    (match fs with
    | [] -> Op op
    | (IField_Field(name,lo,wd) as f)::rest ->
      let vals = field_vals (pprint_ident name) wd in
      let pairs = List.map (fun v -> (v, go (set_field f op v) rest)) vals in
      Field(f, IntMap.of_seq (List.to_seq pairs))
  ) in
  go (int_of_opcode opcode) fields


(* Automatic step-by-step testing of a particular opcode. *)

type operror =
  | Op_EvalFail of exn
  | Op_DisFail of exn
  | Op_DisEvalFail of exn
  | Op_DisEvalNotEqual
  | Op_BnfcFailure

let pp_operror: operror -> string =
  function
  | Op_EvalFail (Throw (loc, Exc_Undefined)) -> "UNDEFINED (" ^ pp_loc loc ^ ")"
  | Op_EvalFail e -> "[1] Evaluation failure: " ^ Printexc.to_string e
  | Op_DisFail e -> "[2] Disassembly failure: " ^ Printexc.to_string e
  | Op_DisEvalFail e -> "[3] Dissassembled evaluation failure: " ^ Printexc.to_string e
  | Op_DisEvalNotEqual -> "[4] Evaluation results not equal"
  | Op_BnfcFailure -> "[5] BNFC parsing of RASL failed"

type 'a opresult = ('a, operror) Result.t

let pp_opresult f = Result.fold ~ok:f ~error:pp_operror

let op_eval (env: Env.t) (iset: string) (op: Primops.bigint): Env.t opresult =
  let evalenv = Env.copy env in
  let decoder = Eval.Env.getDecoder evalenv (Ident iset) in
  try
    Eval.eval_decode_case AST.Unknown evalenv decoder op;
    Result.Ok evalenv
  with
    | e -> Result.Error (Op_EvalFail e)

let op_dis (env: Env.t) (iset: string) (op: Primops.bigint): stmt list opresult =
  let env = Env.copy env in
  let lenv = Dis.build_env env in
  let decoder = Eval.Env.getDecoder env (Ident iset) in
  try
    Dis.check_rasl := true;
    let stmts = Dis.dis_decode_entry env lenv decoder op in
    Result.Ok stmts
  with
    | e -> Result.Error (Op_DisFail e)

let op_try_bnfc (stmts: stmt list): unit opresult =
  (* let prog = "/home/rina/progs/aslp/libASL/x/TestSemantics" in *)
  (* let prog = "cat" in *)
  (* let (stdout, stdin) as proc = Unix.open_process_args prog [| prog |] in *)

  let s = String.concat "\n" @@ List.map
      (fun s -> (Utils.to_string (PP.pp_raw_stmt s)))
      stmts in

let op_diseval (env: Env.t) (stmts: stmt list): Env.t opresult =
  let env = Env.copy env in
  try
    List.iter (Eval.eval_stmt env) stmts;
    Result.Ok env
  with
    | e -> Result.Error (Op_DisEvalFail e)

let op_compare ((evalenv, disenv): Env.t * Env.t): Env.t opresult =
  if Env.compare evalenv disenv then
    Result.Ok evalenv
  else
    Result.Error (Op_DisEvalNotEqual)

let op_test_opcode (env: Env.t) (iset: string) (op: int): Env.t opresult =
  let op = Z.of_int op in

  let initenv = Env.copy env in
  Random.self_init ();
  Eval.randomRegistersAndMemory initenv;
  Eval.initializeGlobals initenv;

  let initenv = Env.freeze initenv in

  let (let*) = Result.bind in
  let* evalenv = op_eval initenv iset op in
  let* disstmts = op_dis env iset op in
  let* _ = op_try_bnfc disstmts in
  let* disevalenv = op_diseval initenv disstmts in
  op_compare (evalenv, disevalenv)

let get_opcodes (opt_verbose: bool ref) (iset: string) (instr: string) (env: Env.t): (string * instr_field list * ((int * bool) list) option) list =
  if !opt_verbose then Printf.printf "Coverage for encoding %s\n%!" instr;

  let re = Str.regexp instr in
  let encoding_matches = function
      | (Encoding_Block (Ident nm, Ident is, _, _, _, _, _, _)) ->
          is = iset && Str.string_match re nm 0
      | _ -> assert false
  in
  let encs = List.map (fun (x,_,_,_) -> x) (Env.listInstructions env) in
  let encs' = List.filter encoding_matches encs in
  (* List.iter (function (Encoding_Block (Ident mn, _,_,_,_,_,_,_)) -> Printf.printf "%s\n" mn | _ -> assert false) encs'; *)

  let opcodes = load_opcodes "encodings" in
  let get_opcodes nm =
      (match opcodes with
      | Some opcodes' -> Option.value (Bindings.find_opt nm opcodes') ~default:[||]
      | None -> [| (0, Int.max_int) |]
  ) in

  (match opcodes with
  | None ->
      Printf.printf "WARNING: encodings/ directory missing, assuming all opcodes are valid.\n";
      Printf.printf "         If encodings.tar.gz exists, it should be extracted.\n\n"
  | Some x ->
      let add_opcodes = Array.to_list (get_opcodes (Ident "ADD_Z_ZI__")) in
      let expected = [(0x2520c000, 0x2520ffff); (0x2560c000,0x2560ffff); (0x25a0c000,0x25a0ffff); (0x25e0c000,0x25e0ffff)] in
      (* check that encodings file has been parsed to correct values.
          if this fails, it is likely your encodings/ directory has the
          incorrect format. *)
      assert (add_opcodes = expected);
      if !opt_verbose then Printf.printf "Loaded opcodes for %d encodings\n" (Bindings.cardinal x)
  );

  List.fold_left (fun encs enc ->
      let Encoding_Block (nm,_,fields,_,_,_,_,_) = enc in
      if !opt_verbose then Printf.printf "Coverage for encoding block %s\n%!" (pprint_ident nm);
      let newEnc = pprint_ident nm in
      let t = enumerate_encoding enc (field_vals_flags_only enc) in
      let l = list_of_enc_tree t in
      let opcodes = (match get_opcodes nm with
      | [||] ->
        None
      | ops ->
          Some (List.fold_left (fun codes op ->
              if pair_array_mem ops op then
                codes @ [(op, true)]
              else
                codes @ [(op, false)]
          ) [] l)
      ) in
      encs @ [(newEnc, fields, opcodes)]
  ) [] encs'
