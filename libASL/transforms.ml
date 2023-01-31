open Asl_utils

open AST
open Visitor
open Asl_visitor
open Symbolic
open Value

(** Remove variables which are unused at the end of the statement list. *)
module RemoveUnused = struct
  let rec remove_unused (globals: IdentSet.t) xs = (remove_unused' globals xs)

  and remove_unused' (used: IdentSet.t) (xs: stmt list): (stmt list) =
    fst @@ List.fold_right (fun stmt (acc, used) ->

      let pass = (acc, used)
      and emit (s: stmt) = (s::acc, IdentSet.union used (fv_stmt s))
      in

      match stmt with
      | Stmt_VarDeclsNoInit(ty, vs, loc) ->
        let vs' = List.filter (fun i -> IdentSet.mem i used) vs in
        (match vs' with
        | [] -> pass
        | _ -> emit (Stmt_VarDeclsNoInit(ty, vs', loc)))
      | Stmt_VarDecl(ty, v, i, loc) ->
        if IdentSet.mem v used
          then emit stmt
          else pass
      | Stmt_ConstDecl(ty, v, i, loc) ->
        if IdentSet.mem v used
          then emit stmt
          else pass
      | Stmt_Assign(le, r, loc) ->
        let lvs = assigned_vars_of_stmts [stmt] in
        if not (IdentSet.disjoint lvs used)
          then emit stmt
          else pass
      | Stmt_If(c, tstmts, elsif, fstmts, loc) ->
        let tstmts' = remove_unused' used tstmts in
        let fstmts' = remove_unused' used fstmts in
        let elsif' = List.map
          (fun (S_Elsif_Cond (c,ss)) ->
            S_Elsif_Cond (c, remove_unused' used ss))
          elsif in
        (match (tstmts',fstmts',elsif') with
        | [], [], [] -> pass
        | _, _, _ -> emit (Stmt_If(c, tstmts', elsif', fstmts', loc)))
      | x -> emit x

    ) xs ([], used)
end



(** Transforms setters using formal reference (in/out) parameters
    into functions returning modified versions of the reference parameters.
*)
module RefParams = struct

  (** Filters the given list of sformal, returning a list of
      (argument index, type, argument name) with only the ref params. *)
  let get_ref_params (xs: sformal list): (int * ty * ident) list =
    let xs = List.mapi (fun i x -> (i,x)) xs in
    List.filter_map
      (fun (n,f) ->
      match f with
      | Formal_InOut (t,i) -> Some (n,t,i)
      | _ -> None)
      xs

  (** Replaces all procedure returns in the given statement list
      with the given statement. *)
  let replace_returns ss s =
    let visit = object
      inherit Asl_visitor.nopAslVisitor
      method! vstmt =
        function
        | Stmt_ProcReturn _ -> ChangeTo s
        | Stmt_FunReturn _ -> failwith "unexpected function return in ref param conversion."
        | _ -> DoChildren
    end
    in
    Asl_visitor.visit_stmts visit ss

  (** Replaces setter declarations which use formal in-out parameters with
      functions which return their modified parameters.

      For example,

        Elem[bits(N) &vector, integer e] = bits(size) value
          ...
          return;

      is transformed to

        (bits(N)) Elem.read(bits(N) vector, integer e, bits(size) value)
          ...
          return (vector);


      *)
  class visit_decls = object
    inherit Asl_visitor.nopAslVisitor

    (* mapping of function identifiers to their (new) signature along with
       the indices of their. *)
    val mutable ref_params : (Tcheck.funtype * int list) Bindings.t = Bindings.empty

    method ref_params = ref_params

    method! vdecl (d: declaration): declaration visitAction =
      match d with
      | Decl_ArraySetterDefn (nm, args, vty, vnm, body, loc)->
        (match get_ref_params args with
        | [] -> DoChildren
        | refs ->
          (* indices, types, and identifiers for the ref params. *)
          let ns = List.map (fun (n,_,_) -> n) refs in
          let ts = List.map (fun (_,t,_) -> t) refs in
          let is = List.map (fun (_,_,i) -> i) refs in

          (* append setter value argument to formal argument list. *)
          let args' = List.map Tcheck.formal_of_sformal args @ [vty, vnm] in

          (* construct return expression to return modified ref vars. *)
          let vars = List.map (fun x -> Expr_Var x) is in
          let ret = Stmt_FunReturn (Expr_Tuple vars, loc) in
          let body' = replace_returns body ret in

          let rty = Type_Tuple ts in
          let funty = (nm, false, [], [], List.map arg_of_sformal args @ [(vty, vnm)], rty) in
          ref_params <- Bindings.add nm (funty,ns) ref_params;
          ChangeTo (Decl_FunDefn (rty, nm, args', body', loc))
        )
      | _ -> DoChildren
  end

  (** Replaces writes to the setters modified above to assign
      the return value back to the original variables.

      For example,

        Elem[vector, 2] = '1001';

      is transformed to

        vector = Elem.read(vector, 2, '1001');

      *)
  class visit_writes (ref_params: (Tcheck.funtype * int list) Bindings.t) = object
    inherit Asl_visitor.nopAslVisitor

    val mutable n = 0;

    method! vstmt (s: stmt): stmt visitAction =
      match s with
      | Stmt_Assign (LExpr_Write (setter, targs, args), r, loc) ->
        (match Bindings.find_opt setter ref_params with
        | None -> DoChildren
        | Some (_,ns) ->
          let refs = List.map (List.nth args) ns in
          (* Printf.printf "ref param: %s\n" (pp_expr a); *)

          let les = List.map Symbolic.expr_to_lexpr refs in
          let call = Expr_TApply (setter, targs, args @ [r]) in
          ChangeTo (Stmt_Assign (LExpr_Tuple les, call, loc))
        )
      (* case where a write expression is used within a tuple destructuring. *)
      | Stmt_Assign (LExpr_Tuple(LExpr_Write (setter, tes, es) :: rest), r, loc) ->
        (match Bindings.find_opt setter ref_params with
        | None -> DoChildren
        | Some ((nm, _, _, _, args, _),ns) ->

          n <- n + 1;
          (* create new variable to store value to be passed to setter. *)
          let rvar = Ident ("Write_" ^ pprint_ident (stripTag setter) ^ string_of_int n) in
          (* arguments to setter function appended with r-value. *)
          let es' = es @ [Expr_Var rvar] in

          (* infer value argument type of setter by substituting arguments into
             the last type argument. *)
          let subs = List.combine (List.map snd args) es' in
          let sub_bindings = Bindings.of_seq (List.to_seq subs) in
          let (vty,_) = List.hd (List.rev args) in
          let vty = subst_type sub_bindings vty in

          (* emit: vty rvar declaration *)
          let decl_var = Stmt_VarDeclsNoInit (vty, [rvar], loc) in
          (* emit: (rvar, ...) = r *)
          let assign_tuple = Stmt_Assign (LExpr_Tuple (LExpr_Var rvar :: rest), r, loc) in

          let refs = List.map (List.nth es') ns in
          let les = List.map Symbolic.expr_to_lexpr refs in
          let write_call = Expr_TApply (setter, tes, es') in
          (* emit: (refparams) = __write(es, rvar) *)
          let assign_write = Stmt_Assign (LExpr_Tuple les, write_call, loc) in

          let x = (Stmt_If (
            expr_true,
            [decl_var; assign_tuple; assign_write],
            [],
            [],
            loc)) in
          ChangeTo x
        )
      | _ -> DoChildren

    method! vlexpr le =
      match le with
      | LExpr_Write (nm, _, _) when Bindings.mem nm ref_params ->
        failwith @@ "unexpected write using parameters by reference: " ^ pp_lexpr le
      | _ -> DoChildren
  end

  let ref_param_conversion (ds: declaration list) =
    let v1 = new visit_decls in
    let ds = List.map (Asl_visitor.visit_decl (v1 :> Asl_visitor.aslVisitor)) ds in
    let v2 = new visit_writes (v1#ref_params) in
    let ds = List.map (Asl_visitor.visit_decl v2) ds in
    ds
    (* Tcheck.GlobalEnv.clear Tcheck.env0;
    Tcheck.tc_declarations false ds *)
end

module StatefulIntToBits = struct
  type interval = (Z.t * Z.t)
  type abs = (int * bool * interval)
  type state = (bool * abs Bindings.t)

  (** Compute the bitvector width needed to represent an interval *)
  let width_of_interval  ?(force_signed=false) ((u,l): interval): int * bool =
    if not force_signed && Z.geq l Z.zero then 
      let i = max (Z.log2up (Z.succ u)) 1 in
      (i,false)
    else
      let u' = if Z.gt u Z.zero then 1 + (Z.log2up (Z.succ u)) else 1 in
      let l' = if Z.lt l Z.zero then 1 + (Z.log2up (Z.neg l)) else 1 in
      (max u' l',true)

  (** Build an abstract point to represent a constant integer *)
  let abs_of_const (c: Z.t): abs =
    let i = (c,c) in
    let (w,s) = width_of_interval i in
    (w,s,i)

  (** Build an abstract point for all values possible in a bv of width w *)
  let abs_of_width (w: int): abs =
    let t = Z.succ (Z.one) in
    let u = Z.pred (Z.pow t (w - 1)) in
    let l = Z.neg (Z.pow t (w - 1)) in
    (w, true, (u,l))

  (** Build an abstract point for unsigned integer in signed representation *)
  let abs_of_uwidth (w: int): abs =
    let t = Z.succ (Z.one) in
    let u = Z.pred (Z.pow t w) in
    let l = Z.zero in
    (w, false, (u,l))

  (* Basic merge of abstract points *)
  let merge_abs ((lw,ls,(l1,l2)): abs) ((rw,rs,(r1,r2)): abs): abs =
    let s = ls || rs in
    let lw = if s && not ls then lw + 1 else lw in
    let rw = if s && not rs then rw + 1 else rw in
    (max lw rw,s,(Z.max r1 l1,Z.min r2 l2))

  (** Max and min of a list of integers *)
  let maxAll (z: Z.t list): Z.t =
    match z with
    | x::xs -> List.fold_left Z.max x xs
    | _ -> invalid_arg ""
  let minAll (z: Z.t list): Z.t =
    match z with
    | x::xs -> List.fold_left Z.min x xs
    | _ -> invalid_arg ""

  (** Brute force the bop and uop cases for range analysis *)
  let bopInterval ((l1,l2): interval) ((r1,r2): interval) (bop: Z.t -> Z.t -> Z.t) =
    (maxAll [bop l1 r1;bop l1 r2;bop l2 r1;bop l2 r2],
     minAll [bop l1 r1;bop l1 r2;bop l2 r1;bop l2 r2])
  let uopInterval ((l1,l2): interval) (uop: Z.t -> Z.t) =
    (maxAll [uop l1;uop l2],
     minAll [uop l1;uop l2])

  (** Preserve abstract points over bops and uops *)
  let abs_of_bop ((lw,ls,li): abs) ((rw,rs,ri): abs) (bop: Z.t -> Z.t -> Z.t): abs =
    let i = bopInterval li ri bop in
    let (iw,s) = width_of_interval ~force_signed:(ls||rs) i in
    let lw = if s && not ls then lw + 1 else lw in
    let rw = if s && not rs then rw + 1 else rw in
    let w = max (max lw rw) iw in
    (w,s,i)
  let abs_of_uop ((lw,ls,li): abs) (uop: Z.t -> Z.t): abs =
    let i = uopInterval li uop in
    let (iw,s) = width_of_interval ~force_signed:ls i in
    let lw = if s && not ls then lw + 1 else lw in
    let w = max lw iw in
    (w,s,i)

  (** Special case the range analysis for division, considering positive and negative denominators *)
  let abs_of_div (num: abs) ((dw,ds,(upper,lower)): abs): abs =
    let abs_of i = abs_of_bop num (dw,ds,i) Primops.prim_zdiv_int in
    let n_one = Z.neg (Z.one) in
    (* Consider ranges from upper to 1 and -1 to lower, excluding 0 *)
    let n_abs = abs_of (Z.min n_one upper, Z.min n_one lower) in
    let p_abs = abs_of (Z.max Z.one upper, Z.max Z.one lower) in
    (* Ignore abstract points that aren't in the denominator's range *)
    if Z.geq lower Z.zero then p_abs (* also captures (0,0) interval *)
    else if Z.leq upper Z.zero then n_abs
    else merge_abs n_abs p_abs

  let width (n,_,_) = n
  let signed (_,s,_) = s
  let interval (_,_,i) = i

  (** Convert abstract point width into exprs & symbols *)
  let expr_of_abs a =
    Expr_LitInt (string_of_int (width a))
  let sym_of_abs a: sym =
    sym_of_int (width a)

  (* Covert an expression and its abstract information to a signed representation *)
  let force_signed (e,old) =
    if signed old then (e,old)
    else
      let abs = (width old + 1, true, interval old) in
      (sym_zero_extend 1 (width old) e, abs)

  (** Extend an expression coupled with its abstract information to a width *)
  let extend (abs) ((e,old) : sym * abs) =
    (* Only extending *)
    assert (width old <= width abs);
    (* Only going from unsigned to signed *)
    assert ((not (signed old)) || signed abs);
    if signed abs && not (signed old) then
      let e = sym_zero_extend 1 (width old) e in
      let w = width old + 1 in
      if w = width abs then e
      else sym_sign_extend (width abs - w) w e
    else if width abs = width old then e 
    else if not (signed abs) then sym_zero_extend (width abs - width old) (width old) e
    else sym_sign_extend (width abs - width old) (width old) e

  let is_power_of_2 n = 
    n <> 0 && 0 = Int.logand n (n-1)

  (** Covert an integer expression tree into a bitvector equivalent *)
  let rec bv_of_int_expr (vars: state) (e: expr): (sym * abs) =
    match e with
    (* Directly translate integer constants into bitvector constants *)
    | Expr_LitInt n
    | Expr_LitHex n ->
        let n = Z.of_string (Value.drop_chars n ' ') in
        let w = abs_of_const n in
        let a = Z.extract n 0 (width w) in
        (sym_of_expr (Expr_LitBits (Z.format ("%0" ^ string_of_int (width w) ^ "b") a)),w)

    (* Assume variables have been declared at this point *)
    | Expr_Var i -> 
        (match Bindings.find_opt i (snd vars) with
        | Some v -> (sym_of_expr e, v)
        | _ -> failwith @@ "bv_of_int_expr: Unknown identifier: " ^ (pprint_ident i))

    | Expr_TApply (FIdent ("cvt_bits_uint", 0), [t], [e]) ->
        let n = int_of_expr t in
        let w = abs_of_uwidth n in
        (sym_of_expr e,w)
    | Expr_TApply (FIdent ("cvt_bits_sint", 0), [t], [e]) ->
        let n = int_of_expr t in
        let w = abs_of_width n in
        (sym_of_expr e,w)

    | Expr_TApply (FIdent ("add_int", 0), [], [x;y]) ->
        let x = bv_of_int_expr vars x in
        let y = bv_of_int_expr vars y in
        let w = abs_of_bop (snd x) (snd y) Primops.prim_add_int in
        let ex = extend w in
        let f = sym_prim (FIdent ("add_bits", 0)) [sym_of_abs w] [ex x;ex y] in
        (f,w)
    | Expr_TApply (FIdent ("sub_int", 0), [], [x;y]) ->
        let x = bv_of_int_expr vars x in
        let y = bv_of_int_expr vars y in
        let w = abs_of_bop (snd x) (snd y) Primops.prim_sub_int in
        let ex = extend w in
        let f = sym_prim (FIdent ("sub_bits", 0)) [sym_of_abs w] [ex x;ex y] in
        (f,w)
    | Expr_TApply (FIdent ("mul_int", 0), [], [x;y]) ->
        let x = bv_of_int_expr vars x in
        let y = bv_of_int_expr vars y in
        let w = abs_of_bop (snd x) (snd y) Primops.prim_mul_int in
        let ex = extend w in
        let f = sym_prim (FIdent ("mul_bits", 0)) [sym_of_abs w] [ex x;ex y] in
        (f,w)

    (* when the divisor is a power of 2, mod can be implemented by truncating. *)
    | Expr_TApply (FIdent ("frem_int", 0), [], [n;Expr_LitInt d]) when is_power_of_2 (int_of_string d) ->
        let digits = Z.log2 (Z.of_string d) in
        let n = bv_of_int_expr vars n in
        if width (snd n) <= digits then n
        else 
          let f = sym_slice Unknown (fst n) 0 digits in
          let w = abs_of_uwidth digits in
          (f,w)

    | Expr_TApply (FIdent ("neg_int", 0), [], [x]) ->
        let x = bv_of_int_expr vars x in
        let w = abs_of_uop (snd x) Primops.prim_neg_int in
        let ex = extend w in
        let f = sym_prim (FIdent ("neg_bits", 0)) [sym_of_abs w] [ex x] in
        (f,w)

    (* TODO: Somewhat haphazard translation from old approach *)
    | Expr_TApply (FIdent ("shl_int", 0), [], [x; y]) -> 
        let x = bv_of_int_expr vars x in
        let y = force_signed (bv_of_int_expr vars y) in
        (match fst y with
        | Val (VBits bv) ->
            let yshift = Z.to_int (Primops.prim_cvt_bits_sint bv) in 
            let size = width (snd x) + yshift in
            let abs = if signed (snd x) then abs_of_width size else abs_of_uwidth size in
            (sym_append_bits Unknown (width (snd x)) yshift (fst x) (sym_zeros yshift),abs)
        | _ -> 
            let (u,_) = interval (snd y) in
            (* in worst case, could shift by 2^(ysize-1)-1 bits, assuming y >= 0. *)
            let size = width (snd x) + (Int.shift_left 2 (Z.to_int u)) - 1 in
            let abs = if signed (snd x) then abs_of_width size else abs_of_uwidth size in
            let ex = extend abs in
            let f = sym_prim (FIdent ("lsl_bits", 0)) [sym_of_int size; sym_of_abs (snd y)] [ex x;fst y] in
            (f,abs)
        )

    (* TODO: Over-approximate range on result, could be a little closer *)
    | Expr_TApply (FIdent ("shr_int", 0), [], [x; y]) -> 
        let x = bv_of_int_expr vars x in
        let y = force_signed (bv_of_int_expr vars y) in
        (sym_prim (FIdent ("asr_bits", 0)) [sym_of_abs (snd x); sym_of_abs (snd y)] [fst x;fst y],snd x)

    | Expr_TApply (FIdent ("round_tozero_real",0), [], [x]) ->
        bv_of_real_expr vars x

    | _ -> failwith @@ "bv_of_int_expr: Unknown integer expression: " ^ (pp_expr e)

  and bv_of_real_expr (vars: state) (e: expr): sym * abs =
    match e with
    | Expr_LitReal n ->
        (* Assume it can be parsed as an integer. TODO: Haven't actually got a bv rep. of a float *)
        bv_of_int_expr vars (Expr_LitInt n)

    | Expr_TApply (FIdent ("divide_real",0), [], [x; y]) ->
        let x = force_signed (bv_of_real_expr vars x) in
        let y = force_signed (bv_of_real_expr vars y) in
        let w = abs_of_div (snd x) (snd y) in
        let ex = extend w in
        let f = sym_prim (FIdent ("sdiv_bits", 0)) [sym_of_abs w] [ex x; ex y] in
        (f,w)

    | Expr_TApply (FIdent ("cvt_int_real", 0), [], [x]) ->
        bv_of_int_expr vars x

    | _ -> failwith @@ "bv_of_real_expr: Unknown real expression: " ^ (pp_expr e)

  let bv_of_int_expr_opt (vars: state) (e: expr): (sym * abs) option =
    try
      Some(bv_of_int_expr vars e)
    with _ -> None

  (** AST traversal to identify the roots of int expr and convert them to bv *)
  class transform_int_expr (vars) = object (self)
    inherit Asl_visitor.nopAslVisitor
    method! vexpr e =
      let e' = match e with
      (* Slice may take bitvector or integer as first argument, allow for failure in bv case *)
      (* TODO: Would prefer to type check x, rather than allowing for failure *)
      | Expr_Slices(x, [Slice_LoWd(l,w)]) ->
          let l = int_of_expr l in
          let w = int_of_expr w in
          (match bv_of_int_expr_opt vars x with
          | Some (e,a) -> 
              if width a = l + w && l = 0 then sym_expr e else
              let x = if width a <= l + w then extend (l+w,signed a,interval a) (e,a) else e in
              sym_expr @@ sym_slice Unknown x l w
          | None -> e)

      (* Other translation from int to bit *)
      | Expr_TApply (FIdent ("cvt_int_bits", 0), [t], [e;_]) -> 
          let e' = bv_of_int_expr vars e in
          let abs = (int_of_expr t,true,(Z.zero,Z.zero)) in
          sym_expr @@ extend abs e'

      | Expr_TApply (FIdent ("eq_int", 0), [], [x;y]) ->
          let x = bv_of_int_expr vars x in
          let y = bv_of_int_expr vars y in
          let w = merge_abs (snd x) (snd y) in
          let ex = extend w in
          sym_expr @@ sym_prim (FIdent ("eq_bits", 0)) [sym_of_abs w] [ex x; ex y]

      | Expr_TApply (FIdent ("ne_int", 0), [], [x;y]) ->
          let x = bv_of_int_expr vars x in
          let y = bv_of_int_expr vars y in
          let w = merge_abs (snd x) (snd y) in
          let ex = extend w in
          sym_expr @@ sym_prim (FIdent ("ne_bits", 0)) [sym_of_abs w] [ex x; ex y]

      (* x >= y  iff  y <= x  iff  x - y >= 0*)
      | Expr_TApply (FIdent ("ge_int", 0), [], [x;y])
      | Expr_TApply (FIdent ("le_int", 0), [], [y;x]) ->
          let x = force_signed (bv_of_int_expr vars x) in
          let y = force_signed (bv_of_int_expr vars y) in
          let w = merge_abs (snd x) (snd y) in
          let ex x = sym_expr (extend w x) in
          expr_prim' "sle_bits" [expr_of_abs w] [ex y;ex x]

      (* x < y  iff  y > x  iff x - y < 0 *)
      | Expr_TApply (FIdent ("lt_int", 0), [], [x;y])
      | Expr_TApply (FIdent ("gt_int", 0), [], [y;x]) ->
          let x = force_signed (bv_of_int_expr vars x) in
          let y = force_signed (bv_of_int_expr vars y) in
          let w = merge_abs (snd x) (snd y) in
          let ex x = sym_expr (extend w x) in
          expr_prim' "slt_bits" [expr_of_abs w] [ex x;ex y]

      (* these functions take bits as first argument and integer as second. just coerce second to bits. *)
      (* TODO: primitive implementations of these expressions expect the shift amount to be signed,
               but a negative shift is invalid anyway. Can't it just be unsigned? *)
      | Expr_TApply (FIdent ("LSL", 0), [size], [x; n]) -> 
          let (n,w) = force_signed (bv_of_int_expr vars n) in
          expr_prim' "lsl_bits" [size; expr_of_abs w] [x;sym_expr n]
      | Expr_TApply (FIdent ("LSR", 0), [size], [x; n]) -> 
          let (n,w) = force_signed (bv_of_int_expr vars n) in
          expr_prim' "lsr_bits" [size; expr_of_abs w] [x;sym_expr n]
      | Expr_TApply (FIdent ("ASR", 0), [size], [x; n]) -> 
          let (n,w) = force_signed (bv_of_int_expr vars n) in
          expr_prim' "asr_bits" [size; expr_of_abs w] [x;sym_expr n]

      | e -> e
      in
      ChangeDoChildrenPost (e', fun e -> e)
  end

  (** Get a variable's abstract rep with a default initial value *)
  let get_default (v: ident) ((_,vars): state): abs =
    match Bindings.find_opt v vars with
    | Some (a,b,_) -> (a,b,(Z.zero,Z.zero))
    | _ -> abs_of_const Z.zero

  (** Declare a new variable with an initial abstract rep *)
  let assign (v: ident) (i: abs) ((f,vars): state): state =
    match Bindings.find_opt v vars with
    | Some j -> 
        (* Entry doesn't change, nothing to do *)
        if i = j then (f,vars)
        (* Same width and sign, but redecl resets range, not a real change *)
        else if width i = width j && signed i = signed j then (f,Bindings.add v i vars)
        else 
          (* Merge width and sign, but keep new range for range analysis *)
          let (w,s,_) = merge_abs i j in
          let m = (w,s,interval i) in
          (true,Bindings.add v m vars)
    | None -> (true,Bindings.add v i vars)

  (** Simple test of existence in state *)
  let tracked (v: ident) ((_,vars): state): bool =
    Bindings.mem v vars

  (** Merge two states at a control flow join *)
  let merge (f1,vars1) (f2,vars2) =
    (f1 || f2, Bindings.merge (fun k l r -> 
      match l, r with
      | Some l, Some r -> Some (merge_abs l r)
      | Some l, None
      | None, Some l -> Some l
      | _, _ -> None) vars1 vars2)

  (** Statement list walk to establish variable widths and visit all expressions *)
  (* 
     TODO: Unique variable names or support multiple decls somehow
     TODO: This won't respect local scopes within If stmts
  *)
  let rec walk (vars: abs Bindings.t) (s: stmt list): (state * stmt list) =
    List.fold_left (fun (st,acc) stmt ->
      let stmt = Asl_visitor.visit_stmt (new transform_int_expr st) stmt in
      let (st,stmt) = (match stmt with

      (* Match integer writes *)
      | Stmt_VarDeclsNoInit(t, [v], loc) when t = type_integer ->
          let lhs = get_default v st in
          let e = Stmt_VarDeclsNoInit (type_bits (string_of_int (width lhs)), [v], loc) in
          let st = assign v lhs st in
          (st,e)
      | Stmt_ConstDecl(t, v, e, loc) when t = type_integer ->
          let lhs = get_default v st in
          let rhs = bv_of_int_expr st e in
          let w = merge_abs lhs (snd rhs) in
          let s = sym_expr (extend w rhs) in
          let s = Stmt_ConstDecl (type_bits (string_of_int (width w)), v, s, loc) in
          let st = assign v w st in
          (st,s)
      | Stmt_VarDecl(t, v, e, loc) when t = type_integer ->
          let lhs = get_default v st in
          let rhs = bv_of_int_expr st e in
          let w = merge_abs lhs (snd rhs) in
          let s = sym_expr (extend w rhs) in
          let s = Stmt_VarDecl (type_bits (string_of_int (width w)), v, s, loc) in
          let st = assign v w st in
          (st,s)
      | Stmt_Assign(LExpr_Var(v), e, loc) when tracked v st ->
          let lhs = get_default v st in
          let rhs = bv_of_int_expr st e in
          let w = merge_abs lhs (snd rhs) in
          let s = sym_expr (extend w rhs) in
          let s = Stmt_Assign (LExpr_Var(v), s, loc) in
          let st = assign v w st in
          (st,s)

      (* Expect only normalised Ifs *)
      | Stmt_If (e, tstmts, [], fstmts, loc) ->
          let (_,vars) = st in
          let (t,tstmts) = walk vars tstmts in
          let (f,fstmts) = walk vars fstmts in
          (merge t f,Stmt_If(e, tstmts, [], fstmts, loc))
      | Stmt_If _ -> failwith "walk: invalid if"

      (* Ignore all other stmts *)
      | Stmt_Assert _ 
      | Stmt_TCall _
      | Stmt_VarDeclsNoInit _ 
      | Stmt_ConstDecl _
      | Stmt_VarDecl _
      | Stmt_Assign _ ->
          (st,stmt)

      | _ -> failwith "walk: invalid IR") in
      (st,acc@[stmt])
    ) ((false,vars),[]) s 

  let rec fixedPoint (vars: abs Bindings.t) (s: stmt list): stmt list =
    let ((changed,vars),res) = walk vars s in
    if changed then begin fixedPoint vars s 
    end else res

  let run (s: stmt list): stmt list =
    fixedPoint Bindings.empty s

end



(** Transforms expressions of integers into equivalent expressions over
    bit-vectors. *)
module IntToBits = struct
  type interval = (Z.t * Z.t)
  let empty_interval = (Z.zero, Z.minus_one)


  (** Returns the number of bits needed to represent n (where n >= 0),
      assuming the bits are interpreted as unsigned. *)
  let num_bits_unsigned n =
    assert (Z.geq n Z.zero);
    if Z.equal n Z.zero then
      0
    else
      Z.log2 n + 1

  (** Returns the number of bits needed to represent n, assuming
      the bits are interpreted as signed two's complement.  *)
  let num_bits_signed n =
    if Z.geq n Z.zero then
      (* non-negative n case is same as unsigned + 1 for sign bit. *)
      num_bits_unsigned n + 1
    else
      (* representing -1 requires representing |n|-1 in
         unsigned, then + 1 for sign bit. *)
      num_bits_unsigned (Z.sub (Z.abs n) Z.one) + 1

  (** Returns the number of (signed) bits needed to represent
      all numbers within (lo,hi) inclusive.  *)
  let size_of_interval (lo,hi) =
    assert (Z.leq lo hi);
    max (max (num_bits_signed lo) (num_bits_signed hi)) 1

  (** Returns the interval which is representable by the given number
      of two's complement bits.  *)
  let interval_of_size (n: int): interval =
    assert (n >= 1);
    let magnitude = Z.shift_left Z.one (n - 1) in
    (Z.neg magnitude, Z.sub magnitude Z.one)

  (** Removes all space characters from the given string. *)
  let drop_space x = Value.drop_chars x ' '

  (** Interprets a bit literal as a signed integer. *)
  let sint_of_bits x =
    let x = Value.drop_chars x ' ' in
    let len = String.length x in
    Z.signed_extract (Z.of_string_base 2 x) 0 len


  (** Returns the bit-width of the given expression.
      Requires expression to evaluate to a bit-vector type. *)
  let rec bits_size_of_expr (vars: ty Bindings.t) (e: expr): int =
    match e with
    | Expr_TApply (fn, tes, es) ->
      (match (fn, tes, es) with
      | FIdent ("cvt_bool_bv", 0), _, _ -> 1
      | FIdent ("add_bits", 0), [Expr_LitInt n], _
      | FIdent ("sub_bits", 0), [Expr_LitInt n], _
      | FIdent ("mul_bits", 0), [Expr_LitInt n], _
      | FIdent ("sdiv_bits", 0), [Expr_LitInt n], _
      | FIdent ("and_bits", 0), [Expr_LitInt n], _
      | FIdent ("or_bits", 0), [Expr_LitInt n], _
      | FIdent ("eor_bits", 0), [Expr_LitInt n], _
      | FIdent ("not_bits", 0), [Expr_LitInt n], _
      | FIdent ("zeros_bits", 0), [Expr_LitInt n], _
      | FIdent ("lsl_bits", 0), [Expr_LitInt n; _], _
      | FIdent ("lsr_bits", 0), [Expr_LitInt n; _], _
      | FIdent ("asr_bits", 0), [Expr_LitInt n; _], _
      | FIdent ("ones_bits", 0), [Expr_LitInt n], _ -> int_of_string n
      | FIdent ("append_bits", 0), [Expr_LitInt n; Expr_LitInt m], _ -> int_of_string n + int_of_string m
      | FIdent ("replicate_bits", 0), [Expr_LitInt n; Expr_LitInt m], _ -> int_of_string n * int_of_string m
      | FIdent ("ZeroExtend", 0), [_; Expr_LitInt m], _
      | FIdent ("SignExtend", 0), [_; Expr_LitInt m], _ -> int_of_string m
      | _ -> failwith @@ "bits_size_of_expr: unhandled " ^ pp_expr e
      )
    | Expr_Parens e -> bits_size_of_expr vars e
    | Expr_LitBits s -> String.length (drop_space s)
    | Expr_Slices (expr, slice_list) ->
      let wds = List.map (function | Slice_LoWd (_,Expr_LitInt n) -> int_of_string n | _ -> assert false) slice_list in
      List.fold_left (+) 0 wds
    | Expr_Var nm ->
      (match Bindings.find_opt nm vars with
      | Some (Type_Bits (Expr_LitInt n)) -> int_of_string n
      | Some t ->
        failwith @@ "bits_size_of_expr: expected bits type but got " ^
        pp_type t ^ " for " ^ pp_expr e
      | None -> failwith @@ "bits_size_of_expr: no type known for " ^ pp_expr e
      )
    | _ -> failwith @@ "bits_size_of_expr: unhandled " ^ pp_expr e

  (** Returns the bit-width of the given value,
      and errors if the value is not a bit value. *)
  let bits_size_of_val (v: value): int =
    match v with
    | VBits {n=n; _} -> n
    | _ -> failwith @@ "bits_size_of_val: unhandled " ^ pp_value v

  (** Returns the bit-width of the given symbolic. *)
  let bits_size_of_sym ?(vars = Bindings.empty)= function
    | Val v -> bits_size_of_val v
    | Exp e -> bits_size_of_expr vars e

  (** Extends the given symbolic to the given size,
      treating it as a signed two's complement expression. *)
  let bits_sign_extend (size: int) (e: sym) =
    let old = bits_size_of_sym e in
    assert (old <= size);
    if old = size
      then e
      else (sym_sign_extend (size - old) old e)

  let bits_coerce_of_expr e =
    let e' =
      match e with
      | Expr_LitInt n
      | Expr_LitHex n ->
        let n' = Z.of_string (drop_space n) in
        let size = num_bits_signed n' in
        let a = Z.extract n' 0 size in
        (Expr_LitBits (Z.format ("%0" ^ string_of_int size ^ "b") a))
      | _ -> e
    in
    sym_of_expr e'

  (** Returns a symbolic bits expression of the given expression
      along with the bit width,
      including coercing integers to two's complement bits where
      needed. *)
  let bits_with_size_of_expr e =
    let e' = bits_coerce_of_expr e in
    e', bits_size_of_sym e'

  let is_power_of_2 n = 
    n <> 0 && 0 = Int.logand n (n-1)

  (** Transform integer expressions into bit-vector expressions while
      maintaining precision by widening bit-vector sizes as operations
      are applied. *)
  class bits_coerce_widening = object (self)
    inherit Asl_visitor.nopAslVisitor

    val no_int_conversion = List.map (fun f -> FIdent (f, 0)) 
      []

    (** Visits an expression, coercing integer expressions into bit-vector
        operations.

        Bit-vectors generated by this conversion are in SIGNED two's complement.
        Each visit case assumes its sub-expressions have already been converted
        to signed bit-vectors.
    *)
    method! vexpr e =
      match e with

      | Expr_TApply (f, _, _) when (List.mem f no_int_conversion) ->
        SkipChildren

        (* match two function calls deep to find truncated division. *)
      | Expr_TApply (FIdent ("round_tozero_real",0), [], 
          [Expr_TApply (FIdent ("divide_real",0), [], args)]) -> 

        ChangeDoChildrenPost (Expr_Tuple args, fun e' -> 
          match e' with 
          | Expr_Tuple [x; y] -> 
            let (x,xsize) = bits_with_size_of_expr x in
            let (y,ysize) = bits_with_size_of_expr y in
            let size = max xsize ysize + 1 in
            let ex = bits_sign_extend size in
            sym_expr @@ sym_prim (FIdent ("sdiv_bits", 0)) [sym_of_int size] [ex x; ex y]
          | _ -> failwith "expected tuple in round divide real case."
        )
        

      | Expr_TApply (fn, tes, es) ->
        ChangeDoChildrenPost (e, fun e' ->
          let unsupported () = 
            failwith @@ "unsupported integer function: " ^ pp_expr e'
          in
          match e' with
          | Expr_TApply (FIdent ("cvt_bits_uint", 0), [t], [e]) ->
            sym_expr @@ sym_zero_extend 1 (int_of_expr t) (bits_coerce_of_expr e)
          | Expr_TApply (FIdent ("cvt_bits_sint", 0), [t], [e]) ->
            (* seemingly unnecessary slices allow inferring the size of 'e'.
               without this, it is impossible in some cases (e.g. if 'e' is a bare variable). *)
            sym_expr @@ sym_slice Unknown (bits_coerce_of_expr e) 0 (int_of_expr t)
          | Expr_TApply (FIdent ("cvt_int_bits", 0), [t], [e;_]) -> 
            let e' = bits_coerce_of_expr e in
            sym_expr @@ bits_sign_extend (int_of_expr t) e'
          | Expr_TApply (FIdent ("add_int", 0), [], [x;y]) ->
            let (x,xsize) = bits_with_size_of_expr x in
            let (y,ysize) = bits_with_size_of_expr y in
            let size = max xsize ysize + 1 in
            let ex = bits_sign_extend size in
            (* Printf.printf "x %s\ny %s\n" (pp_expr x) (pp_expr y) ; *)
            sym_expr @@ sym_prim (FIdent ("add_bits", 0)) [sym_of_int size] [ex x;ex y]

          | Expr_TApply (FIdent ("sub_int", 0), [], [x;y]) ->
            let (x,xsize) = bits_with_size_of_expr x in
            let (y,ysize) = bits_with_size_of_expr y in
            let size = max xsize ysize + 1 in
            let ex = bits_sign_extend size in
            (* Printf.printf "x %s\ny %s\n" (pp_expr x) (pp_expr y) ; *)
            sym_expr @@ sym_prim (FIdent ("sub_bits", 0)) [sym_of_int size] [ex x; ex y]

          | Expr_TApply (FIdent ("eq_int", 0), [], [x;y]) ->
            let (x,xsize) = bits_with_size_of_expr x in
            let (y,ysize) = bits_with_size_of_expr y in
            let size = max xsize ysize in
            let ex = bits_sign_extend size in
            sym_expr @@ sym_prim (FIdent ("eq_bits", 0)) [sym_of_int size] [ex x; ex y]

          | Expr_TApply (FIdent ("ne_int", 0), [], [x;y]) ->
            let (x,xsize) = bits_with_size_of_expr x in
            let (y,ysize) = bits_with_size_of_expr y in
            let size = max xsize ysize in
            let ex = bits_sign_extend size in
            sym_expr @@ sym_prim (FIdent ("ne_bits", 0)) [sym_of_int size] [ex x; ex y]

          | Expr_TApply (FIdent ("mul_int", 0), [], [x;y]) ->
            let (x,xsize) = bits_with_size_of_expr x in
            let (y,ysize) = bits_with_size_of_expr y in
            let size = xsize + ysize in
            let ex = bits_sign_extend size in
            sym_expr @@ sym_prim (FIdent ("mul_bits", 0)) [sym_of_int size] [ex x; ex y]

            (* x >= y  iff  y <= x  iff  x - y >= 0*)
          | Expr_TApply (FIdent ("ge_int", 0), [], [x;y])
          | Expr_TApply (FIdent ("le_int", 0), [], [y;x]) ->
            let (x,xsize) = bits_with_size_of_expr x in
            let (y,ysize) = bits_with_size_of_expr y in
            let size = max xsize ysize in
            let ex x = sym_expr (bits_sign_extend size x) in
            expr_prim' "sle_bits" [expr_of_int size] [ex y;ex x]

            (* x < y  iff  y > x  iff x - y < 0 *)
          | Expr_TApply (FIdent ("lt_int", 0), [], [x;y])
          | Expr_TApply (FIdent ("gt_int", 0), [], [y;x]) ->
            let (x,xsize) = bits_with_size_of_expr x in
            let (y,ysize) = bits_with_size_of_expr y in
            let size = max xsize ysize in
            let ex x = sym_expr (bits_sign_extend size x) in
            expr_prim' "slt_bits" [expr_of_int size] [ex x;ex y]
          (* NOTE: sle_bits and slt_bits are signed less or equal,
             and signed less than.
             These are not primitive in ASL but are defined in BIL so
             we take advantage of them. *)

          | Expr_TApply (FIdent ("neg_int", 0), [], [x]) ->
            let (x,xsize) = bits_with_size_of_expr x in
            let size = xsize + 1 in
            let ex x = sym_expr (bits_sign_extend size x) in
            expr_prim' "neg_bits" [expr_of_int size] [ex x]

          | Expr_TApply (FIdent ("shl_int", 0), [], [x; y]) -> 
            let (x,xsize) = bits_with_size_of_expr x in
            let (y,ysize) = bits_with_size_of_expr y in
            (* in worst case, could shift by 2^(ysize-1)-1 bits, assuming y >= 0. *)
            let size = xsize + Int.shift_left 2 (ysize - 1) - 1 in
            let ex x = sym_expr (bits_sign_extend size x) in
            (match y with 
            | Val (VBits bv) -> 
              (* if shift is statically known, simply append zeros. *)
              let yshift = Z.to_int (Primops.prim_cvt_bits_sint bv) in 
              sym_expr @@ sym_append_bits Unknown xsize yshift x (sym_zeros yshift)
            | _ -> expr_prim' "lsl_bits" [expr_of_int size; expr_of_int ysize] [ex x;sym_expr y]
            )

          | Expr_TApply (FIdent ("shr_int", 0), [], [x; y]) -> 
            let (x,xsize) = bits_with_size_of_expr x in
            let (y,ysize) = bits_with_size_of_expr y in
            let size = xsize in
            let ex x = sym_expr (bits_sign_extend size x) in
            expr_prim' "asr_bits" [expr_of_int size; expr_of_int ysize] [ex x;sym_expr y]

            (* these functions take bits as first argument and integer as second. just coerce second to bits. *)
          | Expr_TApply (FIdent ("LSL", 0), [size], [x; n]) -> 
            let (n,nsize) = bits_with_size_of_expr n in
            expr_prim' "lsl_bits" [size; expr_of_int nsize] [x;sym_expr n]
          | Expr_TApply (FIdent ("LSR", 0), [size], [x; n]) -> 
            let (n,nsize) = bits_with_size_of_expr n in
            expr_prim' "lsr_bits" [size; expr_of_int nsize] [x;sym_expr n]
          | Expr_TApply (FIdent ("ASR", 0), [size], [x; n]) -> 
            let (n,nsize) = bits_with_size_of_expr n in
            expr_prim' "asr_bits" [size; expr_of_int nsize] [x;sym_expr n]

            (* when the divisor is a power of 2, mod can be implemented by truncating. *)
          | Expr_TApply (FIdent ("frem_int", 0), [], [n;Expr_LitInt d]) when is_power_of_2 (int_of_string d) ->
            let digits = Z.log2 (Z.of_string d) in
            let n,_ = bits_with_size_of_expr n in
            sym_expr @@ sym_zero_extend 1 digits (sym_slice Unknown n 0 digits)

            (* very carefully coerce a signed integer to a "real" by just using its signed representation *)
            (* this will only work for particular operations. *)
          | Expr_TApply (FIdent ("cvt_int_real", 0), [], [x]) -> 
            x

          | Expr_TApply (FIdent (f, 0), _, _) when Utils.endswith f "_int" ->
            unsupported ()

          | _ -> e'
        )
    | _ -> DoChildren

  end

  (** A second transform pass which narrows bit-vector expressions which are
      later sliced, by considering the bits needed in that final slice. *)
  class bits_coerce_narrow = object (self)
    inherit Asl_visitor.nopAslVisitor

    val mutable var_types : ty Bindings.t = Bindings.empty;

    method! vstmt s =
      match s with
      | Stmt_ConstDecl(ty, nm, _, _) ->
        var_types <- Bindings.add nm ty var_types;
        DoChildren
      | _ -> DoChildren

    method! vexpr e =
      match e with
      | Expr_Slices(
          Expr_TApply (f, tes, es) as inner,
          [Slice_LoWd (Expr_LitInt lo, Expr_LitInt wd) as sl] ) ->

        let wd' = int_of_string lo + int_of_string wd in
        let narrow e =
          (* Printf.printf "slicing %s\n" (pp_expr e); *)
          let e' = sym_of_expr e in
          let size = bits_size_of_sym ~vars:var_types e' in
          let ext = wd' - size in
          (* if expression is shorter than slice, extend it as needed. *)
          let e' = if ext > 0 then (sym_sign_extend ext size e') else e' in
          if wd' <> size then
            sym_expr @@ sym_slice Unknown e' 0 wd'
          else
            e
        in
        let narrow_args () = Expr_TApply (f, [expr_of_int wd'], List.map narrow es) in

        (* for add and sub expressions, we only need the lowest n bits in order
           to have n bits of precision in the output. *)
        (match name_of_FIdent f with
        | "add_bits" -> ChangeDoChildrenPost (narrow_args (), fun x -> Expr_Slices (x, [sl]))
        | "sub_bits" -> ChangeDoChildrenPost (narrow_args (), fun x -> Expr_Slices (x, [sl]))
        | "neg_bits" -> ChangeDoChildrenPost (narrow_args (), fun x -> Expr_Slices (x, [sl]))
        | _ -> ChangeDoChildrenPost (narrow inner, fun x -> Expr_Slices (x, [sl]))
        )
      | _ -> DoChildren

  end

  let ints_to_bits xs =
    xs
    (*|> Asl_visitor.visit_stmts (new bits_coerce_widening)*)
    |> Asl_visitor.visit_stmts (new bits_coerce_narrow)

end

module CopyProp = struct
  type st = expr Bindings.t
  let debug_cp = false

  (* Extract an access chain for an expr or lexpr, stopping at symbolic indices *)
  let rec get_expr_ac (e: expr): (expr * access_chain list)  =
      match e with
      | Expr_Field (l, f) -> let (l',c) = get_expr_ac l in (l',c@[Field f])
      | Expr_Array (l, Expr_LitInt i) -> let (l',c) = get_expr_ac l in (l',c@[Index (VInt (Z.of_string i))])
      | _ -> (e, []) 
  let rec get_lexpr_ac (le: lexpr): (lexpr * access_chain list)  =
    match le with
    | LExpr_Field (l, f) -> let (l',c) = get_lexpr_ac l in (l',c@[Field f])
    | LExpr_Array (l, Expr_LitInt i) -> let (l',c) = get_lexpr_ac l in (l',c@[Index (VInt (Z.of_string i))])
    | _ -> (le, [])

  (* Identify divergence on access paths to avoid clobbering *)
  let rec overlaps (p: 'a list) (l: 'a list): bool =
    match p, l with
    | x::xs, y::ys -> x = y && overlaps xs ys
    | _ -> true

  (* Clobber walk, determine if assigning to the lexpr may change the expression result *)
  class clobber_walk (cl: lexpr) = object (self)
    inherit nopAslVisitor
    val mutable clobbered = false
    method! vexpr expr =
      match expr with
      | Expr_Var _
      | Expr_Field _
      | Expr_Array _ -> 
          let (lv,lc) = get_lexpr_ac cl in
          let (v,c) = get_expr_ac expr in
          (match lv, v with
          | LExpr_Var lv, Expr_Var v -> 
              (* Clobber if they are the same base variables and lc is a prefix of c *)
              clobbered <- clobbered || (lv = v && overlaps lc c);
              SkipChildren
          | _ ->
              (* Overapprox if base of the operation is not known *)
              if debug_cp then Printf.printf "Copy-Prop over-approx. clobber: %s %s\n" (pp_expr expr) (pp_lexpr cl);
              clobbered <- true;
              SkipChildren)
      | _ -> DoChildren
    method result = clobbered
  end

  let clobber (le: lexpr) (e: expr): bool =
    let visitor = new clobber_walk le in
    let _ = visit_expr visitor e in
    visitor#result

  (* Load walk, identify if the expression is memory dependent *)
  class load_walk = object (self)
    inherit nopAslVisitor
    val mutable clobbered = false
    method! vexpr expr =
      match expr with
      | Expr_TApply (f,_,_) -> 
          clobbered <- clobbered || (name_of_FIdent f = "Mem.read");
          DoChildren
      | _ -> DoChildren
    method result = clobbered
  end

  let load (e: expr): bool =
    let visitor = new load_walk in
    let _ = visit_expr visitor e in
    visitor#result

  let remove (i: ident) (copies: st): st =
    try 
      Bindings.remove i copies
    with _ -> copies

  let removeAll (i: ident list) (copies: st): st =
    List.fold_right remove i copies

  let add (i: ident) (e: expr) (copies: st): st =
    Bindings.add i e copies

  let rec candidateExpr (e: expr): bool =
    match e with
    | Expr_Var v -> true
    | Expr_Field (e,_) -> candidateExpr e
    | Expr_Array (e,_) -> candidateExpr e
    | Expr_TApply (f,_,_) -> (name_of_FIdent f = "Mem.read")
    | _ -> false

  let candidateIdent (i: ident) =
    match i with
    | Ident s -> Str.string_match (Str.regexp "Exp") s 0
    | _ -> false

  let removeClobbers (le: lexpr) (copies: st): st =
    Bindings.filter (fun k e -> not (clobber le e) && not (clobber le (Expr_Var k))) copies

  let removeMemory (copies: st): st =
    Bindings.filter (fun k e -> not (load e)) copies

  let merge (l: st) (r: st): st =
    Bindings.merge (fun k l r -> match l, r with Some l,Some r -> if l = r then Some l else None | _ -> None) l r

  let rec copyProp' (xs: stmt list) (copies: st): (stmt list * st) =
    List.fold_left (fun (acc, copies) stmt ->
      match stmt with
      | Stmt_VarDeclsNoInit(ty, vs, loc) -> 
          (* Clear any redefinitions *)
          (acc@[stmt], removeAll vs copies)

      | Stmt_ConstDecl(_, v, e, loc) 
      | Stmt_VarDecl(_, v, e, loc) ->
          (* Introduce propagations for local decls *)
          let stmt = subst_stmt copies stmt in
          let e = subst_expr copies e in
          let copies = if candidateExpr e then add v e copies else remove v copies in
          (acc@[stmt], copies)

      | Stmt_Assign(le, e, loc) ->
          (* Remove all clobbers *)
          let stmt = subst_stmt copies stmt in
          let copies = removeClobbers le copies in
          let copies = (match le with
          | LExpr_Var(i) -> remove i copies
          | _ -> copies ) in
          (acc@[stmt], copies)

      | Stmt_If (e, tstmts, [], fstmts, loc) ->
          (* Merge if result *)
          let e = subst_expr copies e in
          let (tstmts, tcopies) = copyProp' tstmts copies in
          let (fstmts, fcopies) = copyProp' fstmts copies in
          (acc@[Stmt_If (e, tstmts, [], fstmts, loc)], merge tcopies fcopies)

      | Stmt_Assert (_, _)  ->
          (* Statements that shouldn't clobber *)
          (acc@[subst_stmt copies stmt], copies)

      | Stmt_TCall (FIdent("Mem.set", 0), _, _, _) ->
          (acc@[subst_stmt copies stmt], removeMemory copies)

      | _ -> 
          (* Over-approximate all other situations for soundness *)
          if debug_cp then Printf.printf "Over-approx: %s\n" (pp_stmt stmt);
          (acc@[stmt],Bindings.empty)) 
    ([], copies) xs

  let copyProp (xs: stmt list): stmt list =
    let (acc, _) = copyProp' xs Bindings.empty in
    acc

end

module CommonSubExprElim = struct
  (* Basic common sub-expression elimination.
     (Theoretical) Pitfalls:
     - Type inference of our factorised subexpressions isn't great. See large match statement in infer_cse_expr_type
     - Eliminating two connected expressions will depend entirely on which one it sees first.
        i.e. trying to simultaneously factorise "add (mem (add (3+4)))" and "mem (add (3+4))" - 
        ideal case is "x = mem (add (3+4)); y = add (x)" but this may not happen.
     - We only attempt to eliminate TApplys. TApplys are our "primitive functions" and are the
        main goal of this transform but we could also eliminate many other things.
  *)
  exception CSEError of string

  class gather_expressions = object
    inherit Asl_visitor.nopAslVisitor

    val mutable exprs: expr list = ([]: expr list);
    val mutable cand_exprs: expr list = ([]: expr list);

    method! vexpr (e: expr): expr visitAction = 
      let () = match e with
      (* For now, only gather TApply's that we've seen more than once
         See eval_prim in value.ml for the list of what that covers. *)
      | Expr_TApply(_) ->
        if (List.mem e cand_exprs) && not (List.mem e exprs) then 
          exprs <- e :: exprs
        else cand_exprs <- e :: cand_exprs;
      | _ ->
        ()
      in
      DoChildren
    
    method get_info: expr list = 
      exprs
  end

  class replace_all_instances = object
    inherit Asl_visitor.nopAslVisitor

    val mutable candidates: (expr * ident) list = []
    val mutable do_replace: bool = true
    
    method! vexpr (e: expr): expr visitAction = 
      let valid_replacement (e: expr): ident option =
        let found = List.filter (fun a -> fst a = e) candidates in
        if List.length found = 1 then
          Some (snd (List.nth found 0))
        else
          None
      in

      let result = match (valid_replacement e) with
      | Some i ->
        if do_replace then ChangeTo(Expr_Var(i)) else DoChildren
      | None ->
        DoChildren
      in
      result
      
    method! vstmt (s: stmt): stmt visitAction = 
      let () = match s with
      | Stmt_ConstDecl(_, Ident(n), _, Unknown) when (Str.string_match (Str.regexp "Cse") n 0) ->
        do_replace <- false
      | _ ->
        do_replace <- true
      in DoChildren

    method add (name: ident) (value: expr) =
      candidates <- (value, name)::candidates
  end 

  let infer_cse_expr_type (e: expr): ty = 
    match e with
    | Expr_TApply((FIdent(name, _) | Ident(name)), [], _) -> begin
      match name with
      | "eq_enum"            -> type_bool
      | "ne_enum"            -> type_bool
      | "eq_bool"            -> type_bool
      | "ne_bool"            -> type_bool
      | "equiv_bool"         -> type_bool
      | "not_bool"           -> type_bool
      | "eq_int"             -> type_bool
      | "ne_int"             -> type_bool
      | "le_int"             -> type_bool
      | "lt_int"             -> type_bool
      | "ge_int"             -> type_bool
      | "gt_int"             -> type_bool
      | "is_pow2_int"        -> type_bool
      | "neg_int"            -> type_integer
      | "add_int"            -> type_integer
      | "sub_int"            -> type_integer
      | "shl_int"            -> type_integer
      | "shr_int"            -> type_integer
      | "mul_int"            -> type_integer
      | "zdiv_int"           -> type_integer
      | "zrem_int"           -> type_integer
      | "fdiv_int"           -> type_integer
      | "frem_int"           -> type_integer
      | "mod_pow2_int"       -> type_integer
      | "align_int"          -> type_integer
      | "pow2_int"           -> type_integer
      | "pow_int_int"        -> type_integer
      | "eq_real"            -> type_bool
      | "ne_real"            -> type_bool
      | "le_real"            -> type_bool
      | "lt_real"            -> type_bool
      | "ge_real"            -> type_bool
      | "round_tozero_real"  -> type_integer
      | "round_down_real"    -> type_integer
      | "round_up_real"      -> type_integer
      | "in_mask"            -> type_bool
      | "notin_mask"         -> type_bool
      | "eq_str"             -> type_bool
      | "ne_str"             -> type_bool
      | "is_cunpred_exc"     -> type_bool
      | "is_exctaken_exc"    -> type_bool
      | "is_impdef_exc"      -> type_bool
      | "is_see_exc"         -> type_bool
      | "is_undefined_exc"   -> type_bool
      | "is_unpred_exc"      -> type_bool
      | "asl_file_open"      -> type_integer
      | "asl_file_getc"      -> type_integer
      | "cvt_bool_bv"        -> Type_Bits(Expr_LitInt("1"))
      | "cvt_bv_bool"        -> type_bool
      | _ -> raise (CSEError ("Can't infer type of strange primitive: " ^ (pp_expr e)))
      end
    | Expr_TApply((FIdent(name, _) | Ident(name)), [Expr_LitInt(_) as num], _) -> begin
      match name with
      | "ram_read"           -> Type_Bits(num)
      | "add_bits"           -> Type_Bits(num)
      | "sub_bits"           -> Type_Bits(num)
      | "mul_bits"           -> Type_Bits(num)
      | "and_bits"           -> Type_Bits(num)
      | "or_bits"            -> Type_Bits(num)
      | "eor_bits"           -> Type_Bits(num)
      | "not_bits"           -> Type_Bits(num)
      | "neg_bits"           -> Type_Bits(num)
      | "zeros_bits"         -> Type_Bits(num)
      | "ones_bits"          -> Type_Bits(num)
      | "replicate_bits"     -> Type_Bits(num)
      | "append_bits"        -> Type_Bits(num)
      | "cvt_int_bits"       -> Type_Bits(num)
      | "LSL"                -> Type_Bits(num)
      | "LSR"                -> Type_Bits(num)
      | "ASR"                -> Type_Bits(num)
      | "cvt_bits_uint"      -> type_integer
      | "cvt_bits_sint"      -> type_integer
      | "eq_bits"            -> type_bool
      | "ne_bits"            -> type_bool
      | _ -> raise (CSEError ("Can't infer type of strange primitive: " ^ (pp_expr e)))
      end
    | Expr_TApply((FIdent(name, _) | Ident(name)), [Expr_LitInt(v1) as num1; Expr_LitInt(v2) as num2], _) -> begin
      (* These are... dubious. None appear in value.ml, so they're all based on what "looks correct". *)
      match name with
      | "ZeroExtend"         -> Type_Bits(num2)
      | "SignExtend"         -> Type_Bits(num2)
      | "lsl_bits"           -> Type_Bits(num1)
      | "lsr_bits"           -> Type_Bits(num1)
      | "asl_bits"           -> Type_Bits(num1)
      | "asr_bits"           -> Type_Bits(num1)
      | "append_bits"        -> 
        Type_Bits(Expr_LitInt(string_of_int((int_of_string v1) + (int_of_string v2))))
      | _ -> raise (CSEError ("Can't infer type of strange primitive: " ^ (pp_expr e)))
      end
    | _ -> 
      raise (CSEError ("Can't infer type of strange expr: " ^ (pp_expr e)))
  
  let insert_into_stmts (xs: stmt list) (x: stmt): (stmt list) =
    let rec move_after_stmts (head: stmt list) (tail: stmt list) (targets: IdentSet.t) (found: IdentSet.t) = 
      if IdentSet.subset targets found then 
        (head, tail)
      else 
        match tail with
          | [] -> raise (CSEError "Couldn't find all vars from CSE target!")
          | next::all ->
            (* "find" the sets of free variables *and* the sets of assigned variables.
               theoretically assigned should be enough but i'm not sure if we might have the case
               where we want to eliminate an expression that directly uses registers, which aren't assigned *)
            let newfound = IdentSet.union found (IdentSet.union (assigned_vars_of_stmts [next]) (fv_stmt next)) in
            move_after_stmts (head @ [next]) all targets newfound
    in

    let targets = IdentSet.filter (fun a -> 
      match a with
      | Ident(s) ->
        (* make sure we're not looking for the actual name of our CSE value *)
        not (Str.string_match (Str.regexp "Cse") s 0)
      | _ -> false
    ) (fv_stmt x) in
    let lists = move_after_stmts [] xs targets (IdentSet.empty) in

    (fst lists) @ [x] @ (snd lists)
  
  let apply_knowledge (xs: stmt list) (knowledge: expr list) (repl): (stmt list) = 
    let rec add_exprs_num (xs: stmt list) (k: expr list) (id: int) = 
      match k with
      | [] -> xs
      | head::tail -> 
        let new_var_name = "Cse" ^ string_of_int id ^ "__5" in
        (* It would be nice to infer the type of the new CSE value *)
        let new_stmt = Stmt_ConstDecl(infer_cse_expr_type head, Ident(new_var_name), head, Unknown) in

        let () = repl#add (Ident(new_var_name)) head in
        add_exprs_num (insert_into_stmts xs new_stmt) (tail) (id+1)
    in
    add_exprs_num xs knowledge 0

  let rec gain_info_pass (xs: stmt list) (knowledge: expr list) (n: int): (expr list) = 
    if (n >= List.length xs) then knowledge else (
      gain_info_pass xs knowledge (n+1)
    )
  
  let do_transform (xs: stmt list): stmt list = 
    let expression_visitor = new gather_expressions in
    let expression_replacer = new replace_all_instances in

    let xs = visit_stmts expression_visitor xs in
    let xs = apply_knowledge xs expression_visitor#get_info expression_replacer in
    let xs = visit_stmts expression_replacer xs in
    xs
end
