(****************************************************************
 * Abstract Interpreter
 ****************************************************************)

module TC = Tcheck

open Asl_ast
open Asl_utils
open Primops
open Abstract_interface

module Make (V: Value) (I: Effect with type value = V.value ) =  struct
  open V
  open I

  (** Monad Utilities *)
  let pure = I.pure
  let (>>=) = I.(>>=)
  let (>>) = I.(>>)

  let unit = pure ()
  let (let*) = I.(>>=)
  let (let+) = I.(>>)
  let (>>>) = fun a b -> a >>= (fun _ -> b)

  let traverse_ f a =
    let+ _ = traverse f a in ()

  let traverse2 (f: 'a -> 'b -> 'c eff) (a: 'a list) (b: 'b list): 'c list eff =
    traverse (fun (a,b) -> f a b) (List.combine a b)

  (** Boolean Utilities *)
  let vtrue = V.from_bool true
  let vfalse = V.from_bool false

  let short_and (x: value eff) (y: value eff): value eff =
    let* c = x in branch c y (pure vfalse)

  let short_or (x: value eff) (y: value eff): value eff =
    let* c = x in branch c (pure vtrue) y

  let short_imp (x: value eff) (y: value eff): value eff =
    let* c = x in branch c y (pure vtrue)

  let rec for_all2 p l1 l2 =
    match (l1, l2) with
    | ([], []) -> pure vtrue
    | (a1::l1, a2::l2) -> short_and (p a1 a2) (for_all2 p l1 l2)
    | (_, _) -> invalid_arg "for_all2"

  let rec exists p = function
    | [] -> pure vfalse
    | [a] -> p a
    | a::l -> short_or (p a) (exists p l)

  (** Bitvector Utilities *)
  let extract_bits_int (loc: l) (v: value) (l: int) (w: int): value =
    extract_bits loc v (from_int l) (from_int w)

  let removeGlobalConsts (ids: IdentSet.t): IdentSet.t eff =
    let+ f = isGlobalConstFilter in
    IdentSet.filter (fun id -> not (f id)) ids

  (** Evaluate bitslice of instruction opcode *)
  let eval_decode_slice (loc: l) (x: decode_slice) (op: value): value eff =
    (match x with
    | DecoderSlice_Slice (lo, wd) -> 
        pure (extract_bits_int loc op lo wd)
    | DecoderSlice_FieldName f -> 
        getVar loc f
    | DecoderSlice_Concat fs -> 
        let+ vs = traverse (getVar loc) fs in
        concat loc vs
    )

  (** Evaluate instruction decode pattern match *)
  let rec eval_decode_pattern (loc: l) (x: decode_pattern) (op: value): value =
    (match x with
    | DecoderPattern_Bits     b -> eq     loc op (from_bitsLit b)
    | DecoderPattern_Mask     m -> inmask loc op (from_maskLit m)
    | DecoderPattern_Wildcard _ -> vtrue
    | DecoderPattern_Not      p -> not_bool loc (eval_decode_pattern loc p op)
    )

  (** Evaluate pattern match *)
  let rec eval_exprs (loc: l) (xs: expr list): (value list) eff =
    traverse (eval_expr loc) xs

  and eval_pattern (loc: l) (v: value) (x: pattern): value eff =
    match x with
    | Pat_LitInt(l)  -> pure (eq     loc v (from_intLit l))
    | Pat_LitHex(l)  -> pure (eq     loc v (from_hexLit l))
    | Pat_LitBits(l) -> pure (eq     loc v (from_bitsLit l))
    | Pat_LitMask(l) -> pure (inmask loc v (from_maskLit l))
    | Pat_Const(c)   -> getGlobalConst c >> eq loc v
    | Pat_Wildcard   -> pure vtrue
    | Pat_Tuple(ps) ->
        let vs = to_tuple loc v in
        assert (List.length vs = List.length ps);
        for_all2 (eval_pattern loc) vs ps
    | Pat_Set(ps) ->
        exists (eval_pattern loc v) ps
    | Pat_Single(e) ->
        eval_expr loc e >> eq loc v
    | Pat_Range(lo, hi) ->
        let* lo' = eval_expr loc lo in
        let+ hi' = eval_expr loc hi in
        and_bool loc (leq_int loc lo' v) (leq_int loc v hi')

  and eval_slice (loc: l) (x: slice): (value * value) eff =
    match x with
    | Slice_Single(i) ->
        let+ i' = eval_expr loc i in
        (i', from_int 1)
    | Slice_HiLo(hi, lo) ->
        let* hi' = eval_expr loc hi in
        let+ lo' = eval_expr loc lo in
        (lo', add_int loc (sub_int loc hi' lo') (from_int 1))
    | Slice_LoWd(lo, wd) ->
        let* lo' = eval_expr loc lo in
        let+ wd' = eval_expr loc wd in
        (lo', wd')

  and eval_expr (loc: l) (x: expr): value eff =
    match x with
    | Expr_If(c, t, els, e) ->
        let rec eval_if xs d = match xs with
        | [] -> eval_expr loc d
        | E_Elsif_Cond (cond, b)::xs' ->
            let* g = eval_expr loc cond in
            branch g (* then *)
              (eval_expr loc b)
            (* else *)
              (eval_if xs' d)
        in
        eval_if (E_Elsif_Cond(c, t)::els) e
    | Expr_Binop(a, op, b) ->
        error loc @@ "binary operation should have been removed in expression "
                   ^ Utils.to_string (PP.pp_expr x)
    | Expr_Field(e, f) ->
        let+ v = eval_expr loc e in
        get_field loc v f
    | Expr_Fields(e, fs) ->
        let+ v  = eval_expr loc e in
        let vs = List.map (get_field loc v) fs in
        concat loc vs
    | Expr_Slices(e, ss) ->
        let* v  = eval_expr loc e in
        let+ vs = traverse (fun s ->
          let+ (i, w) = eval_slice loc s in
          extract_bits loc v i w
        ) ss in
        concat loc vs
    | Expr_In(e, p) ->
        let* v = eval_expr loc e in
        eval_pattern loc v p
    | Expr_Var(v) ->
        getVar loc v
    | Expr_Parens(e) ->
        eval_expr loc e 
    | Expr_TApply(f, tes, es) ->
        (* First deal with &&, || and IMPLIES all of which only evaluate
         * their second argument if they need to
         *)
        if name_of_FIdent f = "and_bool" then begin
          match (tes, es) with
          | ([], [x; y]) -> short_and (eval_expr loc x) (eval_expr loc y)
          | _ ->
              error loc @@ "malformed and_bool expression "
                   ^ Utils.to_string (PP.pp_expr x)
        end else if name_of_FIdent f = "or_bool" then begin
          match (tes, es) with
          | ([], [x; y]) -> short_or (eval_expr loc x) (eval_expr loc y)
          | _ ->
              error loc @@ "malformed or_bool expression "
                   ^ Utils.to_string (PP.pp_expr x)
        end else if name_of_FIdent f = "implies_bool" then begin
          match (tes, es) with
          | ([], [x; y]) -> short_imp (eval_expr loc x) (eval_expr loc y)
          | _ ->
              error loc @@ "malformed imp_bool expression "
                   ^ Utils.to_string (PP.pp_expr x)
        end else begin
          let* tvs = eval_exprs loc tes in
          let* vs  = eval_exprs loc es in
          eval_funcall loc f tvs vs
        end
    | Expr_Tuple(es) ->
        let+ vs = eval_exprs loc es in
        from_tuple vs
    | Expr_Unop(op, e) ->
        error loc @@ "unary operation should have been removed" (* static error *)
    | Expr_Unknown(t) ->
        eval_unknown loc t
    | Expr_ImpDef(t, Some(s)) ->
        getImpdef loc s
    | Expr_ImpDef(t, None) ->
        error loc @@ "unnamed IMPLEMENTATION_DEFINED behavior" (* static error *)
    | Expr_Array(a, i) ->
        let* a' = eval_expr loc a in
        let+ i' = eval_expr loc i in
        get_array loc a' i'
    | Expr_LitInt(i) ->    pure (from_intLit i)
    | Expr_LitHex(i) ->    pure (from_hexLit i)
    | Expr_LitReal(r) ->   pure (from_realLit r)
    | Expr_LitBits(b) ->   pure (from_bitsLit b)
    | Expr_LitMask(b) ->   pure (from_maskLit b) (* todo: masks should not be expressions *)
    | Expr_LitString(s) -> pure (from_stringLit s)

  (** Evaluate L-expression in write-mode (i.e., this is not a read-modify-write) *)
  and eval_lexpr (loc: l) (x: lexpr) (r: value): unit eff =
    match x with
    | LExpr_Wildcard ->
        unit
    | LExpr_Var(v) ->
        setVar loc v r
    | LExpr_Field(l, f) ->
        eval_lexpr_modify loc l (fun prev -> pure (set_field loc prev f r))
    | LExpr_Fields(l, fs) ->
        let rec set_fields (i: value) (fs: ident list) (prev: value): value eff =
          (match fs with
          | [] -> pure prev
          | (f::fs') ->
              let p = get_field loc prev f in (* read previous value to get width *)
              let w = width_bits loc p in
              let y = extract_bits loc r i w in
              let v' = set_field loc prev f y in
              let n = add_int loc i w in
              set_fields n fs' v'
          )
        in
        eval_lexpr_modify loc l (set_fields (from_int 0) (List.rev fs))
    | LExpr_Slices(l, ss) ->
        let rec eval (o: value) (ss': slice list) (prev: value): value eff =
          (match ss' with
          | [] -> pure prev
          | (s :: ss) ->
              let* (i, w) = eval_slice loc s in
              let v       = extract_bits loc r o w in
              eval (add_int loc o w) ss (insert_bits loc prev i w v)
          )
        in
        eval_lexpr_modify loc l (eval (from_int 0) (List.rev ss))
    | LExpr_BitTuple(ls) ->
        failwith "eval_lexpr: bittuple"
    | LExpr_Tuple(ls) ->
        let rs = to_tuple loc r in
        assert (List.length ls = List.length rs);
        let+ _ = traverse2 (eval_lexpr loc) ls rs in
        ()
    | LExpr_Array(l, i) ->
        let* i' = eval_expr loc i in
        eval_lexpr_modify loc l (fun prev -> pure (set_array loc prev i' r))
    | LExpr_Write(setter, tes, es) ->
        let* tvs = eval_exprs loc tes in
        let* vs  = eval_exprs loc es in
        eval_proccall loc setter tvs (List.append vs [r])
    | _ ->
        failwith ("eval_lexpr: "^ (pp_lexpr x))

  (** Evaluate L-expression in read-modify-write mode.

      1. The old value of the L-expression is read.
      2. The function 'modify' is applied to the old value
      3. The result is written back to the L-expression.
   *)
  and eval_lexpr_modify (loc: l) (x: lexpr) (modify: value -> value eff): unit eff =
    match x with
    | LExpr_Var(v) ->
        let* old = getVar loc v in
        let* n = modify old in
        setVar loc v n
    | LExpr_Field(l, f) ->
        let modify' (prev: value): value eff =
          let old = get_field loc prev f in
          let+ n = modify old in
          set_field loc prev f n
        in
        eval_lexpr_modify loc l modify'
    | LExpr_Array(l, i) ->
        let* i' = eval_expr loc i in
        let modify' (prev: value): value eff =
          let old = get_array loc prev i' in
          let+ n = modify old in
          set_array loc prev i' n
        in
        eval_lexpr_modify loc l modify'
    | LExpr_ReadWrite (getter, setter, tes, es) ->
        let* tvs = eval_exprs loc tes in
        let* vs  = eval_exprs loc es in
        let* old = eval_funcall loc getter tvs vs in
        let* n = modify old in
        eval_proccall loc setter tvs (List.append vs [n])
    | _ -> failwith "eval_lexpr_modify"

  (** Evaluate list of statements *)
  and eval_stmts (xs: stmt list): unit eff =
    scope (traverse_ eval_stmt xs)

  (** Evaluate statement *)
  and eval_stmt (x: stmt): unit eff =
    match x with
    | Stmt_VarDeclsNoInit(ty, vs, loc) ->
        traverse_ (fun v -> eval_unknown loc ty >>= addLocalVar loc v) vs
    | Stmt_VarDecl(ty, v, i, loc) ->
        let* i' = eval_expr loc i in
        addLocalVar loc v i'
    | Stmt_ConstDecl(ty, v, i, loc) ->
        let* i' = eval_expr loc i in
        addLocalConst loc v i'
    | Stmt_Assign(l, r, loc) ->
        let* r' = eval_expr loc r in
        eval_lexpr loc l r'
    | Stmt_TCall(f, tes, es, loc) ->
        let* tvs = eval_exprs loc tes in
        let* vs  = eval_exprs loc es in
        eval_proccall loc f tvs vs
    | Stmt_FunReturn(e, loc) ->
        let* v = eval_expr loc e in
        return v
    | Stmt_ProcReturn(loc) ->
        return vunit
    | Stmt_Assert(e, loc) ->
        let* v = eval_expr loc e in
        branch v (* then *)
          unit
        (* else *)
          (error loc "assertion failure") (* runtime error *)
    | Stmt_Unpred(loc) ->
        throw loc Exc_Unpredictable
    | Stmt_ConstrainedUnpred(loc) ->
        throw loc Exc_ConstrainedUnpredictable
    | Stmt_ImpDef(v, loc) ->
        throw loc @@ Exc_ImpDefined (pprint_ident v)
    | Stmt_Undefined(loc) ->
        throw loc Exc_Undefined
    | Stmt_ExceptionTaken(loc) ->
        throw loc Exc_ExceptionTaken
    | Stmt_Dep_Unpred(loc) ->
        throw loc Exc_Unpredictable
    | Stmt_Dep_ImpDef(s, loc) ->
        throw loc @@ Exc_ImpDefined s
    | Stmt_Dep_Undefined(loc) ->
        throw loc Exc_Undefined
    | Stmt_See(e, loc) ->
        let* v = eval_expr loc e in
        throw loc @@ Exc_SEE (to_string loc v)
    | Stmt_Throw(v, loc) ->
        let* v = getVar loc v in
        let (l,e) = to_exc loc v in
        throw l e
    | Stmt_DecodeExecute(i, e, loc) ->
        let* dec = getDecoder i in
        let* op  = eval_expr loc e in
        eval_decode_case loc dec op
    | Stmt_If(c, t, els, e, loc) -> 
        let rec eval css d =
          match css with
          | [] -> eval_stmts d
          | (S_Elsif_Cond(c, s) :: css') ->
              let* g = eval_expr loc c in
              branch g (* then *)
                (eval_stmts s)
              (* else *)
                (eval css' d)
        in
        scope (eval (S_Elsif_Cond(c, t) :: els) e)
    | Stmt_Case(e, alts, odefault, loc) ->
        let rec eval v alts =
          (match alts with
          | [] ->
              (match odefault with
              | None -> error loc "unmatched case" (* runtime error *)
              | Some s -> eval_stmts s
              )
          | (Alt_Alt(ps, None, s) :: alts') ->
              let* g = exists (eval_pattern loc v) ps in
              branch g (* then *)
                (eval_stmts s)
              (* else *)
                (eval v alts')
          | (Alt_Alt(ps, Some c, s) :: alts') ->
              let c1 = exists (eval_pattern loc v) ps in
              let c2 = eval_expr loc c in
              let* g = short_and c1 c2 in
              branch g (* then *)
                (eval_stmts s)
              (* else *)
                (eval v alts')
          )
        in
        let* v = eval_expr loc e in
        scope (eval v alts)
    | Stmt_For(v, start, dir, stop, b, loc) ->
        let* start' = eval_expr loc start in
        let* stop'  = eval_expr loc stop in
        let body i =
          let c = (match dir with
          | Direction_Up   -> leq_int loc i stop'
          | Direction_Down -> leq_int loc stop' i
          ) in
          branch c (* then *)
            (scope (addLocalVar loc v i >>> eval_stmts b) >>>
            let i' = (match dir with
            | Direction_Up   -> add_int loc i (from_int 1)
            | Direction_Down -> sub_int loc i (from_int 1)
            ) in
            pure (i', vtrue))
          (* else *)
            (pure (i, vfalse))
        in
        let+ _ = iter body start' in
        ()
    | Stmt_While(c, b, loc) ->
        iter (fun _ ->
          let* g = eval_expr loc c in
          branch g (eval_stmts b >>> pure ((),vtrue)) (pure ((),vfalse))) ()
    | Stmt_Repeat(b, c, loc) ->
        iter (fun _ ->
          let* () = eval_stmts b in
          let+ g = eval_expr loc c in ((),g)) ()
    | Stmt_Try(tb, ev, catchers, odefault, loc) ->
        catch (eval_stmts tb) (fun l ex -> scope (
          let rec eval cs = 
            match cs with
            | [] ->
                (match odefault with
                | None   -> throw l ex
                | Some s -> eval_stmts s)
            | (Catcher_Guarded(c, b) :: cs') ->
                let* c = eval_expr loc c in
                branch c (eval_stmts b) (eval cs')
          in
          addLocalVar loc ev (from_exc l ex) >>>
          eval catchers) )

  (** Evaluate call to function or procedure *)
  and eval_call (loc: l) (f: ident) (tvs: value list) (vs: value list): value eff =
    let* r = runPrim (name_of_FIdent f) tvs vs in
    (match r with
    | Some r -> pure r
    | None ->
        begin
          let* (rty, atys, targs, args, loc, b) = getFun loc f in
          assert (List.length targs = List.length tvs);
          assert (List.length args  = List.length vs);
          call (
            traverse2 (fun arg v -> addLocalVar loc arg v) targs tvs >>>
            traverse2 (fun arg v -> addLocalVar loc arg v) args vs >>>
            eval_stmts b
          )
        end
    )

  (** Evaluate call to function *)
  and eval_funcall (loc: l) (f: ident) (tvs: value list) (vs: value list): value eff =
    let* r = eval_call loc f tvs vs in
    if is_unit r then error loc "no return statement" else pure r

  (** Evaluate call to procedure *)
  and eval_proccall (loc: l) (f: ident) (tvs: value list) (vs: value list): unit eff =
    let* r = eval_call loc f tvs vs in
    if is_unit r then unit else error loc "value return from proc"

  (** Evaluate instruction decode case *)
  and eval_decode_case (loc: l) (x: decode_case) (op: value): unit eff =
    match x with
    | DecoderCase_Case (ss, alts, loc) ->
        let* vs = traverse (fun s -> eval_decode_slice loc s op) ss in
        let rec eval alts =
          (match alts with
          | (alt::alts') -> 
              let* g = eval_decode_alt loc alt vs op in
              branch g unit (eval alts')
          | [] -> error loc "unmatched decode pattern"
          )
        in
        eval alts

  (** Evaluate instruction decode case alternative *)
  and eval_decode_alt (loc: l) (DecoderAlt_Alt (ps, b)) (vs: value list) (op: value): value eff =
    let* g = for_all2 (fun a b -> pure (eval_decode_pattern loc a b)) ps vs in
    branch g (* then *) begin
      match b with
      | DecoderBody_UNPRED loc -> throw loc Exc_Unpredictable
      | DecoderBody_UNALLOC loc -> throw loc Exc_Undefined
      | DecoderBody_NOP loc -> pure (vtrue)
      | DecoderBody_Encoding (enc, l) ->
          let* (enc, opost, cond, exec) = getInstruction loc enc in
          let* g = eval_encoding enc op in
          branch g (* then *) begin
            traverse eval_stmt (match opost with Some p -> p | None -> []) >>>
            (* todo: should evaluate ConditionHolds to decide whether to execute body *)
            traverse eval_stmt exec >>>
            pure vtrue
          end (* else *) (pure vfalse)
      | DecoderBody_Decoder (fs, c, loc) ->
          (* let env = Env.empty in  *)
          traverse (function (IField_Field (f, lo, wd)) ->
            addLocalVar loc f (extract_bits_int loc op lo wd)
          ) fs >>>
          eval_decode_case loc c op >>>
          pure (vtrue)
    end (* else *) (pure vfalse)

  (** Evaluate instruction encoding *)
  and eval_encoding (x: encoding) (op: value): value eff =
    let Encoding_Block (nm, iset, fields, opcode, guard, unpreds, b, loc) = x in
    (* todo: consider checking iset *)
    (* Printf.printf "Checking opcode match %s == %s\n" (Utils.to_string (PP.pp_opcode_value opcode)) (pp_value op); *)
    let ok = (match opcode with
    | Opcode_Bits b -> eq     loc op (from_bitsLit b)
    | Opcode_Mask m -> inmask loc op (from_maskLit m)
    ) in
    branch ok (* then *) begin
      traverse (function (IField_Field (f, lo, wd)) ->
        let v = extract_bits_int loc op lo wd in
        addLocalVar loc f v
      ) fields >>>
      let* g = eval_expr loc guard in
      branch g (* then *) begin
        traverse (fun (i, b) ->
          branch (eq loc (extract_bits_int loc op i 1) (from_bitsLit b))
            (throw loc Exc_Unpredictable)
            unit
        ) unpreds >>>
        traverse eval_stmt b >>>
        pure vtrue
      end (* else *)  (pure vfalse)
    end (* else *) (pure vfalse)

  (****************************************************************)
  (** {2 Creating environment from global declarations}           *)
  (****************************************************************)
  
  (* Uninitialized global variables are UNKNOWN by default *)
  and eval_unknown (loc: l) (x: ty): value eff =
    match x with
    | Type_Constructor(Ident "integer") -> pure (unknown_integer loc)
    | Type_Constructor(Ident "real")    -> pure (unknown_real loc)
    | Type_Constructor(Ident "string")  -> pure (unknown_string loc)
    | Type_Constructor(tc) ->
        let* es = getEnum tc in
        (match es with
        | Some e -> pure (unknown_enum loc e)
        | None ->
            let* fs = getRecord tc in
            (match fs with
            | Some f ->
                let+ vs = traverse (fun (ty, f) -> let+ v = eval_unknown loc ty in (f,v)) f in
                new_record vs
            | None ->
                let* ts = getTypedef tc in
                (match ts with
                | Some ty' -> eval_unknown loc ty'
                | None -> error loc @@ "eval_unknown " ^ Utils.to_string (PP.pp_ty x)
                )
            )
        )
    | Type_Bits(n) ->
        let+ w = eval_expr loc n in
        unknown_bits loc w
    | Type_App(Ident "__RAM", [a]) ->
        let+ a' = eval_expr loc a in
        unknown_ram loc a'
    | Type_App(tc, es) ->
        error loc @@ "eval_unknown App " ^ Utils.to_string (PP.pp_ty x)
    | Type_OfExpr(e) ->
        error loc @@ "eval_unknown typeof " ^ Utils.to_string (PP.pp_ty x)
    | Type_Register(wd, _) -> 
        pure (unknown_bits loc (from_intLit wd))
    | Type_Array(Index_Enum(_),ety) 
    | Type_Array(Index_Range(_,_),ety) ->
        let+ v = eval_unknown loc ety in
        new_array v
    | Type_Tuple(tys) ->
        let+ vs = traverse (eval_unknown loc) tys in
        from_tuple vs

  (** Construct environment from global declarations *)
  let build_evaluation_environment (ds: declaration list): unit eff = begin
    reset >>>
    (* todo?: first pull out the constants/configs and evaluate all of them
     * lazily?
     *)
    traverse_ (fun d ->
      (match d with
      | Decl_Record (v, fs, loc) ->
          addRecord v fs
      | Decl_Enum(qid, es, loc) ->
          let evs = if qid = Ident "boolean" then begin (* optimized special case *)
            [ (Ident "FALSE", vfalse); (Ident "TRUE", vtrue) ]
          end else begin
            List.mapi (fun i e -> (e, from_enum e i)) es;
          end
          in
          traverse_ (fun (e, v) -> addGlobalConst e v) evs >>>
          addEnum qid (List.map (fun (e, v) -> v) evs)
      | Decl_Typedef (v, ty, loc) ->
          addTypedef v ty
      | Decl_Var(ty, v, loc) ->
          let* init = eval_unknown loc ty in
          addGlobalVar v init
      | Decl_Const(ty, v, i, loc) ->
          (* todo: constants need to be lazily evaluated or need to be
           * sorted by dependencies
           *)
          let* init = eval_expr loc i in
          addGlobalConst v init
      | Decl_FunDefn(rty, f, atys, body, loc) ->
          let* t = removeGlobalConsts (TC.fv_funtype (f, false, [], [], atys, rty)) in
          let tvs  = Asl_utils.to_sorted_list t in
          let args = List.map snd atys in
          addFun loc f (Some rty, atys, tvs, args, loc, body)
      | Decl_ProcDefn(f, atys, body, loc) ->
          let* t = removeGlobalConsts (Asl_utils.fv_args atys) in
          let tvs  = Asl_utils.to_sorted_list t in
          let args = List.map snd atys in
          addFun loc f (None, atys, tvs, args, loc, body)
      | Decl_VarGetterDefn(ty, f, body, loc) ->
          let* t = removeGlobalConsts (Asl_utils.fv_type ty) in
          let tvs  = Asl_utils.to_sorted_list t in
          let args = [] in
          addFun loc f (Some ty, [], tvs, args, loc, body)
      | Decl_ArrayGetterDefn(rty, f, atys, body, loc) ->
          let* t = removeGlobalConsts (TC.fv_funtype (f, true, [], [], atys, rty)) in
          let tvs = Asl_utils.to_sorted_list t in
          let args = List.map snd atys in
          addFun loc f (Some rty, atys, tvs, args, loc, body)
      | Decl_VarSetterDefn(f, ty, v, body, loc) ->
          let* t = removeGlobalConsts (Asl_utils.fv_type ty) in
          let tvs  = Asl_utils.to_sorted_list t in
          let args = [v] in
          addFun loc f (Some ty, [], tvs, args, loc, body)
      | Decl_ArraySetterDefn(f, atys, ty, v, body, loc) ->
          let ts = Asl_utils.IdentSet.union (Asl_utils.fv_sformals atys) (Asl_utils.fv_type ty) in
          let* t = removeGlobalConsts ts in
          let tvs = Asl_utils.to_sorted_list t in
          let tuple_of (x: AST.sformal): ty * ident =
              (match x with
              | Formal_In (t, nm) -> t,nm
              | Formal_InOut (t, nm) -> t,nm
              )
          in
          (* Add value parameter for setter to end of arguments. *)
          let atys' = List.map tuple_of atys @ [(ty, v)] in
          let args = List.map snd atys' in
          addFun loc f (None, atys', tvs, args, loc, body)
      | Decl_InstructionDefn(nm, encs, opost, conditional, exec, loc) ->
          (* Instructions are looked up by their encoding name *)
          traverse_ (fun enc ->
            let Encoding_Block (nm, _, _, _, _, _, _, _) = enc in
            addInstruction loc nm (enc, opost, conditional, exec)
          ) encs
      | Decl_DecoderDefn(nm, case, loc) ->
          addDecoder nm case
      | Decl_NewMapDefn(rty, f, atys, body, loc) ->
          let* t = removeGlobalConsts (TC.fv_funtype (f, false, [], [], atys, rty)) in
          let tvs  = Asl_utils.to_sorted_list t in
          let args = List.map snd atys in
          addFun loc f (Some rty, atys, tvs, args, loc, body)
      (*
      | Decl_MapClause(f, atys, cond, body, loc) ->
              let tvs   = Asl_utils.to_sorted_list (Asl_utils.fv_args atys) in
              let args' = List.map snd args in
              Env.addFun loc env f (tvs, args', loc, body)
      *)
      | Decl_NewEventDefn (f, atys, loc) ->
          let* t = removeGlobalConsts (Asl_utils.fv_args atys) in
          let tvs = Asl_utils.to_sorted_list t in
          let args = List.map snd atys in
          addFun loc f (None, atys, tvs, args, loc, [])
      | Decl_EventClause (f, body, loc) ->
          let* (_, _, tvs, args, _, body0) = getFun loc f in
          addFun loc f (None, [], tvs, args, loc, List.append body body0)
      (* todo: when creating initial environment, should pass in a set of configuration
       * options that will override any default values given in definition
       *)
      | Decl_Config(ty, v, i, loc) ->
          (* todo: config constants need to be lazily evaluated or need to be
           * sorted by dependencies
           *)
          let* init = eval_expr loc i in
          addGlobalConst v init
  
      (* The following declarations have no impact on execution *)
      | Decl_BuiltinType (_, _)           | Decl_Forward (_, _)
      | Decl_BuiltinFunction (_, _, _, _)
      | Decl_FunType (_, _, _, _)         | Decl_ProcType (_, _, _)
      | Decl_VarGetterType (_, _, _)      | Decl_ArrayGetterType (_, _, _, _)
      | Decl_VarSetterType (_, _, _, _)   | Decl_ArraySetterType (_, _, _, _, _)
      | Decl_Operator1 (_, _, _)
      | Decl_Operator2 (_, _, _)
      | Decl_MapClause (_, _, _, _, _)
      -> unit
      )
      ) ds
  end

end
