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


module DecoderChecks = struct
  type sl = (int * int)
  type st = {
    ctx: MLBDD.man;
    vars: sl Bindings.t;
    cur_enc: MLBDD.t;
    unpred: MLBDD.t;
    unalloc: MLBDD.t;
    nop: MLBDD.t;
    instrs: MLBDD.t Bindings.t;
  }

  let init_state =
    let ctx = MLBDD.init ~cache:1024 () in
    {
      ctx ;
      vars = Bindings.empty ;
      cur_enc = MLBDD.dtrue ctx ;
      unpred = MLBDD.dfalse ctx ;
      unalloc = MLBDD.dfalse ctx ;
      nop = MLBDD.dfalse ctx ;
      instrs = Bindings.empty ;
    }

  let get_slice s st  = Bindings.find s st.vars

  let extract_field (IField_Field(f, lo, wd)) st =
    { st with vars = Bindings.add f (lo,wd) st.vars }

  let add_unpred g st =
    { st with unpred = MLBDD.dor st.unpred g }

  let add_unalloc g st =
    { st with unalloc = MLBDD.dor st.unalloc g }

  let add_nop g st =
    { st with nop = MLBDD.dor st.nop g }

  let add_instr k g st =
    let existing = Option.value (Bindings.find_opt k st.instrs) ~default:(MLBDD.dfalse st.ctx) in
    { st with instrs = Bindings.add k (MLBDD.dor existing g) st.instrs }

  let restrict_enc g st =
    { st with cur_enc = MLBDD.dand st.cur_enc g }

  let set_enc g st =
    { st with cur_enc = g }

  let bdd_of_mask bs lo ctx =
    snd @@ String.fold_right (fun c (pos,e) ->
      match c with
      | ' ' -> (pos, e)
      | 'x' -> (* No constraint *)
          (pos + 1, e)
      | '1' -> (* bit hi is true *)
        let bit = MLBDD.ithvar ctx pos in
        (pos + 1, MLBDD.dand bit e)
      | '0' -> (* bit hi is true *)
        let bit = MLBDD.dnot (MLBDD.ithvar ctx pos) in
        (pos + 1, MLBDD.dand bit e)
      | _ -> invalid_arg "bdd_of_mask") bs (lo,MLBDD.dtrue ctx)

  let implicant_to_mask m =
    let chars = List.init 32 (fun i ->
      if List.mem (true, i)  m then '1' else
        if List.mem (false, i) m then '0' else
          'x'
        ) in
    let buf = Buffer.create 32 in
    List.iter (Buffer.add_char buf) (List.rev chars);
    Buffer.contents buf

  let to_string bdd =
    let imps = MLBDD.allprime bdd in
    Utils.pp_list implicant_to_mask imps

  (* Represent slices in terms of their position in enc *)
  let decode_slice s st =
    match s with
    | DecoderSlice_Slice(lo, wd) -> (lo,wd)
    | DecoderSlice_FieldName f   -> get_slice f st
    | DecoderSlice_Concat fs     -> failwith "DecoderSlice_Concat not expected"

  (* Convert decode patterns into BDDs *)
  let rec decode_pattern (lo,wd) p ctx =
    match p with
    | DecoderPattern_Bits b
    | DecoderPattern_Mask b -> bdd_of_mask b lo ctx
    | DecoderPattern_Wildcard _ -> MLBDD.dtrue ctx
    | DecoderPattern_Not p -> MLBDD.dnot (decode_pattern (lo,wd) p ctx)

  (* Combine the various tests due to a guard into one BDD *)
  let decode_case_guard vs ps st =
    List.fold_left2 (fun e s p -> MLBDD.dand e (decode_pattern s p st.ctx)) (st.cur_enc) vs ps

  (* Collect reachability for each instruction encoding IGNORING ordering on alts *)
  let rec tf_decode_case b st =
    match b with
    | DecoderBody_UNPRED loc          -> add_unpred st.cur_enc st
    | DecoderBody_UNALLOC loc         -> add_unalloc st.cur_enc st
    | DecoderBody_NOP loc             -> add_nop st.cur_enc st
    | DecoderBody_Encoding(nm, loc)   -> add_instr nm st.cur_enc st
    | DecoderBody_Decoder(fs, c, loc) ->
        tf_decoder c (List.fold_right extract_field fs st)

  and tf_decoder (DecoderCase_Case(ss, alts, loc)) st =
    let vs = List.map (fun s -> decode_slice s st) ss in
    List.fold_left ( fun st (DecoderAlt_Alt(ps,b))->
      let guard = decode_case_guard vs ps st in
      let st' = tf_decode_case b (restrict_enc guard st) in
      let st = set_enc st.cur_enc st' in
      st ) st alts

  (* *)
  let possibly_set pos ctx nm bdd =
    let res = MLBDD.(dand bdd (ithvar ctx pos)) in
    not (MLBDD.is_false res)

  (* *)
  let possibly_clr pos ctx nm bdd =
    let res = MLBDD.(dand bdd (dnot (ithvar ctx pos))) in
    not (MLBDD.is_false res)

  (*
  let rec generate_bit_split pos reach st =
    Printf.printf "%d\n" pos;
    if pos < 20 || Bindings.cardinal st.instrs < 2 then
      (match Bindings.bindings st.instrs with
      | (nm,bdd)::xs -> [Stmt_TCall(nm, [], [], Unknown)]
      | [] -> [])
    else
      let set = Bindings.filter (possibly_set pos st.ctx) st.instrs in
      let clr = Bindings.filter (possibly_clr pos st.ctx) st.instrs in
      let set_reach = MLBDD.(dand reach (ithvar st.ctx pos)) in
      let clr_reach = MLBDD.(dand reach (dnot (ithvar st.ctx pos))) in
      let set_split = generate_bit_split (pos - 1) set_reach {st with instrs = set} in
      let clr_split = generate_bit_split (pos - 1) clr_reach {st with instrs = clr} in
      let test = Expr_TApply(FIdent("eq_bits",0), [Expr_LitInt "1"], [Expr_Slices( Expr_Var (Ident "enc"), [Slice_LoWd(expr_of_int pos,expr_of_int 1)]); Expr_LitBits "1"]) in
      [Stmt_If(test, set_split, [], clr_split, Unknown)]
      *)

  let generate_decoder st =
    let stmts = Bindings.fold (fun nm bdd stmts ->
      let imps = MLBDD.allprime bdd in
      let masks = List.map implicant_to_mask imps in
      let body = [Stmt_TCall(get_body_fn nm, [], [], Unknown)] in
      stmts@(List.map (fun m -> S_Elsif_Cond(expr_in_mask (Expr_Var enc) m, body)) masks)) st.instrs [] in
    match stmts with
    | S_Elsif_Cond(c,b)::els -> [Stmt_If(c,b,els,[],Unknown)]
    | _ -> failwith "unreachable"

  let do_transform d env =
    let st = tf_decoder d init_state in
    generate_decoder st

end


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
  let dec_body = DecoderChecks.do_transform decoder env in
  let fid = FIdent(iset ^ "_decoder", 0) in
  let typed_args = generate_args include_pc in
  let dec = (fid, (None, typed_args, [], List.map snd typed_args, loc, dec_body)) in

  (* Build the encoding test functions if necessary *)
  let (trivial,essen) = List.partition is_trivial_test encs in
  let tests = List.map build_test_fn essen in
  (*let has_test = IdentSet.of_list ( List.map ( fun ((Encoding_Block (nm, _, fields, opcode, guard, unpreds, b, loc)),opost,cond,exec) -> nm ) essen ) in*)

  (* Build the instruction functions *)
  let instr = List.map (build_instr_fn include_pc) encs in

  (* Build the decoder itself *)
  (*let dec = build_decoder include_pc has_test iset decoder loc in*)

  (* Add to the environment *)
  List.iter (fun (f,s) -> Eval.Env.addFun loc env f s) tests;
  List.iter (fun (f,s) -> Eval.Env.addFun loc env f s) instr;
  List.iter (fun (f,s) -> Eval.Env.addFun loc env f s) [dec];

  (* Return the decoder, test functions and instruction behaviours *)
  (dec,bindings_of_list tests,bindings_of_list instr)
