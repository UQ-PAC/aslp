open Asl_ast
open Symbolic
open Asl_utils

let enc = Ident("enc")
let enc_type = Type_Bits (expr_of_int 32)
let pc = Ident("pc")
let pc_type = Type_Bits (expr_of_int 32)

let generate_args include_pc =
  [enc_type, enc] @ (if include_pc then [pc_type, pc] else [])

let expr_in_bits e b =
  let bv = Value.to_bits Unknown (Value.from_bitsLit b) in
  Expr_TApply (FIdent("eq_bits", 0), [Expr_LitInt (string_of_int bv.n)], [e; Expr_LitBits b])

let expr_in_mask e b =
  let bv = Value.to_mask Unknown (Value.from_maskLit b) in
  sym_expr @@ sym_inmask Unknown (Exp e) bv
let get_body_fn nm = FIdent (pprint_ident nm, 0)


(*
  let mutual e bdds =
    let distinct = List.mapi (fun i e' -> (i,MLBDD.(is_false (dand e e')))) bdds in
    List.filter_map (fun (i,b) -> if not b then Some i else None) distinct

  let rec order_indep pos bdds =
    match bdds with
    | e::xs ->
        let res = mutual e xs in
        let res = List.map (fun offset -> (pos, offset + pos)) res in
        let remaining = order_indep (pos+1) xs in
        res@remaining
    | [] -> []


  (* Given a list of bodies and their BDD guards, collapse common bodies *)
  let similar_bodies a b =
    match a, b with
    | DecoderBody_UNPRED _, DecoderBody_UNPRED _ -> true
    | DecoderBody_UNALLOC _, DecoderBody_UNALLOC _ -> true
    | DecoderBody_NOP _, DecoderBody_NOP _ -> true
    | DecoderBody_Encoding(nm, _), DecoderBody_Encoding(nm', _) -> nm = nm'
    | _ -> false

  let rec common_bodies xs =
    match xs with
    | (g,b)::xs ->
        let (same,rest) = List.partition (fun (_,b') -> similar_bodies b b') xs in
        let conds = List.map (fun (g,_) -> g) same in
        let collapse = List.fold_right MLBDD.dor conds g in
        let rest = common_bodies rest in
        (collapse,b)::rest
    | _ -> []

  let slice_of_mask m (lo,wd) =
    let hi = 32 - lo - wd in
    DecoderPattern_Mask (String.sub m hi wd)

  let covert_back e slices =
    let masks = List.map implicant_to_mask (MLBDD.allprime e) in
    List.map (fun m -> List.map (slice_of_mask m) slices) masks
*)




(*
  Convert an ASL decoder/instruction construct into an executable ASL program.
  The results consists of:
    - A decoder function
    - A series of instruction encoding test functions, to sanity check the result
    - A series of instruction encoding behaviour functions, corresponding to the instruction execution

  The decoder and instruction behaviour functions take the 32bit instruction encoding and optionally
  the current PC, returning nothing.
  The test functions take only the current instruction encoding and return a boolean result.
*)

let enc_expr opcode =
  match opcode with
  | Opcode_Bits b -> expr_in_bits (Expr_Var enc) b
  | Opcode_Mask m ->
      if String.exists (fun c -> c = 'x') m then expr_in_mask (Expr_Var enc) m
      else expr_in_bits (Expr_Var enc) m

let enc_slice lo wd =
  Expr_Slices (Expr_Var enc, [Slice_LoWd (expr_of_int lo, expr_of_int wd)])

let field_extract loc (IField_Field (f, lo, wd)) =
  Stmt_ConstDecl (Type_Bits (expr_of_int wd), f, enc_slice lo wd, loc)

let unpred_test loc (i, b) =
  Stmt_Assert (Expr_TApply (FIdent ("ne_bits", 0), [expr_of_int 1], [enc_slice i 1; Expr_LitBits b]), loc)

let not_expr a = Expr_TApply (FIdent ("not_bool", 0), [], [a])

let rec and_exprs = function
  | [e] -> e
  | e::es -> Expr_TApply (FIdent ("and_bool", 0), [], [e;and_exprs es])
  | [] -> expr_true

let decode_slice_expr s =
  match s with
  | DecoderSlice_Slice(lo, wd) -> enc_slice lo wd
  | DecoderSlice_FieldName f   -> Expr_Var f
  | DecoderSlice_Concat fs     -> failwith "DecoderSlice_Concat not expected"

let rec decode_pattern_expr p e =
  match p with
  | DecoderPattern_Bits b     -> expr_in_bits e b
  | DecoderPattern_Mask b     -> expr_in_mask e b
  | DecoderPattern_Wildcard _ -> expr_true
  | DecoderPattern_Not p      -> not_expr (decode_pattern_expr p e)

(*
   Test function to evaluate guard statements on instruction encodings.
   Often these tests are trivially true, avoid building the function if so.
   No need to sanity test the opcode, we validate this in a pre-pass over the decoder statically.
 *)
let get_test_fn nm = FIdent (pprint_ident nm ^ "_decode_test", 0)
let is_trivial_test ((Encoding_Block (nm, _, fields, opcode, guard, unpreds, b, loc)),opost,cond,exec) =
  unpreds = [] && guard = expr_true
let build_test_fn ((Encoding_Block (nm, _, fields, opcode, guard, unpreds, b, loc)),opost,cond,exec) =
  (* Return the guard result *)
  let stmts = [Stmt_FunReturn(guard, loc)] in
  (* Assert no unpredictable bits *)
  let stmts = List.map (unpred_test loc) unpreds @ stmts in
  let fid = get_test_fn nm in
  (fid, (Some type_bool, [enc_type, enc], [], [enc], loc, stmts))

let build_instr_fn include_pc ((Encoding_Block (nm, _, fields, opcode, guard, unpreds, b, loc)),opost,cond,exec) =
  (* Extract all of the instructions fields *)
  let stmts = List.map (field_extract loc) fields in
  (* Add encoding body *)
  let stmts = stmts @ b in
  (* Add post encoding body *)
  let stmts = stmts @ (match opost with Some b -> b | _ -> []) in
  (* Add execution body *)
  let stmts = stmts @ exec in
  (* Build the function decl *)
  let fid = get_body_fn nm in
  let typed_args = generate_args include_pc in
  (fid, (None, typed_args, [], List.map snd typed_args, loc, stmts))

let rec decode_case include_pc has_test vs (DecoderAlt_Alt (ps, b)) =
  let ps = List.map2 decode_pattern_expr ps vs in
  let (body, oc) = (match b with
  | DecoderBody_UNPRED loc ->  ([Stmt_Dep_Unpred(loc)], [])
  | DecoderBody_UNALLOC loc -> ([Stmt_Undefined(loc)], [])
  | DecoderBody_NOP loc -> ([], [])
  | DecoderBody_Encoding(nm, loc) ->
      let test_fn = get_test_fn nm in
      let body_fn = get_body_fn nm in
      let args = (Expr_Var enc)::(if include_pc then [Expr_Var pc] else []) in
      let test = Expr_TApply (test_fn, [], [Expr_Var enc]) in
      ([Stmt_TCall(body_fn, [], args, loc)], if IdentSet.mem nm has_test then [test] else [])
  | DecoderBody_Decoder (fs, c, loc) ->
      let stmts = List.map (field_extract loc) fs in
      (stmts @ build_decoder_case include_pc has_test c, [])) in
  let c = and_exprs (ps @ oc) in
  S_Elsif_Cond(c, body)

and build_decoder_case include_pc has_test (DecoderCase_Case(ss, alts, loc)) =
  let decode_slices = List.map decode_slice_expr ss in
  match List.map (decode_case include_pc has_test decode_slices) alts with
  | S_Elsif_Cond(c,body)::xs -> [Stmt_If(c, body, xs, [Stmt_Assert(expr_false,loc)], loc)]
  | _ -> failwith "Empty decoder case"

let build_decoder include_pc has_test iset c loc =
  let stmts = build_decoder_case include_pc has_test c in
  let fid = FIdent(iset ^ "_decoder", 0) in
  let typed_args = generate_args include_pc in
  (fid, (None, typed_args, [], List.map snd typed_args, loc, stmts))

let run include_pc iset pat env =
  let loc = Unknown in

  (* Find all matching instructions, pulled from testing.ml *)
  let decoder = Eval.Env.getDecoder env (Ident iset) in
  let re = Pcre.regexp pat in
  let filterfn = function
    | ((Encoding_Block (Ident nm, Ident is, _, _, _, _, _, _)),_,_,_) ->
        is = iset && Pcre.pmatch ~rex:re nm
    | _ -> assert false
  in
  let encs = List.filter filterfn (Eval.Env.listInstructions env) in

  (* Run a series of sanity tests over the decoder *)
  (*let dec_body = DecoderChecks.do_transform decoder env in
  let fid = FIdent(iset ^ "_decoder", 0) in
  let typed_args = generate_args include_pc in
  let dec = (fid, (None, typed_args, [], List.map snd typed_args, loc, dec_body)) in
  *)

  (* Build the encoding test functions if necessary *)
  let (trivial,essen) = List.partition is_trivial_test encs in
  let tests = List.map build_test_fn essen in
  let has_test = IdentSet.of_list ( List.map ( fun ((Encoding_Block (nm, _, fields, opcode, guard, unpreds, b, loc)),opost,cond,exec) -> nm ) essen ) in

  (* Build the instruction functions *)
  let instr = List.map (build_instr_fn include_pc) encs in

  (* Build the decoder itself *)
  let dec = build_decoder include_pc has_test iset decoder loc in

  (* Add to the environment *)
  List.iter (fun (f,s) -> Eval.Env.addFun loc env f s) tests;
  List.iter (fun (f,s) -> Eval.Env.addFun loc env f s) instr;
  List.iter (fun (f,s) -> Eval.Env.addFun loc env f s) [dec];

  (* Return the decoder, test functions and instruction behaviours *)
  (dec,bindings_of_list tests,bindings_of_list instr)
