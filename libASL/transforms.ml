open Asl_utils

open AST
open Visitor
open Asl_visitor
open Symbolic
open Value

(* TODO: Central definition of prims in result + sanity test pass *)
let pure_prims =
  (List.map (fun f -> FIdent(f,0)) Value.prims_pure) @
  [
    FIdent("SignExtend",0);
    FIdent("ZeroExtend",0);
    FIdent("asr_bits",0);
    FIdent("lsr_bits",0);
    FIdent("lsl_bits",0);
    FIdent("slt_bits",0);
    FIdent("sle_bits",0);
  ]

let infer_type (e: expr): ty option =
  match e with
  | Expr_Slices(x, [Slice_LoWd(l,w)]) -> Some(Type_Bits(w))
  | Expr_TApply((FIdent(name, _) | Ident(name)), [], _) -> begin
    match name with
    | "eq_enum"            -> Some(type_bool)
    | "ne_enum"            -> Some(type_bool)
    | "eq_bool"            -> Some(type_bool)
    | "ne_bool"            -> Some(type_bool)
    | "and_bool"           -> Some(type_bool)
    | "or_bool"            -> Some(type_bool)
    | "equiv_bool"         -> Some(type_bool)
    | "not_bool"           -> Some(type_bool)
    | "eq_int"             -> Some(type_bool)
    | "ne_int"             -> Some(type_bool)
    | "le_int"             -> Some(type_bool)
    | "lt_int"             -> Some(type_bool)
    | "ge_int"             -> Some(type_bool)
    | "gt_int"             -> Some(type_bool)
    | "is_pow2_int"        -> Some(type_bool)
    | "neg_int"            -> Some(type_integer)
    | "add_int"            -> Some(type_integer)
    | "sub_int"            -> Some(type_integer)
    | "shl_int"            -> Some(type_integer)
    | "shr_int"            -> Some(type_integer)
    | "mul_int"            -> Some(type_integer)
    | "zdiv_int"           -> Some(type_integer)
    | "zrem_int"           -> Some(type_integer)
    | "fdiv_int"           -> Some(type_integer)
    | "frem_int"           -> Some(type_integer)
    | "mod_pow2_int"       -> Some(type_integer)
    | "align_int"          -> Some(type_integer)
    | "pow2_int"           -> Some(type_integer)
    | "pow_int_int"        -> Some(type_integer)
    | "eq_real"            -> Some(type_bool)
    | "ne_real"            -> Some(type_bool)
    | "le_real"            -> Some(type_bool)
    | "lt_real"            -> Some(type_bool)
    | "ge_real"            -> Some(type_bool)
    | "round_tozero_real"  -> Some(type_integer)
    | "round_down_real"    -> Some(type_integer)
    | "round_up_real"      -> Some(type_integer)
    | "in_mask"            -> Some(type_bool)
    | "notin_mask"         -> Some(type_bool)
    | "eq_str"             -> Some(type_bool)
    | "ne_str"             -> Some(type_bool)
    | "is_cunpred_exc"     -> Some(type_bool)
    | "is_exctaken_exc"    -> Some(type_bool)
    | "is_impdef_exc"      -> Some(type_bool)
    | "is_see_exc"         -> Some(type_bool)
    | "is_undefined_exc"   -> Some(type_bool)
    | "is_unpred_exc"      -> Some(type_bool)
    | "asl_file_open"      -> Some(type_integer)
    | "asl_file_getc"      -> Some(type_integer)
    | "cvt_bool_bv"        -> Some(Type_Bits(Expr_LitInt("1")))
    | "cvt_bv_bool"        -> Some(type_bool)
    | _ -> None
    end
  | Expr_TApply((FIdent(name, _) | Ident(name)), [Expr_LitInt(_) as num], _) -> begin
    match name with
    | "ram_read"           -> Some(Type_Bits(num))
    | "add_bits"           -> Some(Type_Bits(num))
    | "sub_bits"           -> Some(Type_Bits(num))
    | "mul_bits"           -> Some(Type_Bits(num))
    | "sdiv_bits"          -> Some(Type_Bits(num))
    | "and_bits"           -> Some(Type_Bits(num))
    | "or_bits"            -> Some(Type_Bits(num))
    | "eor_bits"           -> Some(Type_Bits(num))
    | "not_bits"           -> Some(Type_Bits(num))
    | "zeros_bits"         -> Some(Type_Bits(num))
    | "ones_bits"          -> Some(Type_Bits(num))
    | "replicate_bits"     -> Some(Type_Bits(num))
    | "append_bits"        -> Some(Type_Bits(num))
    | "cvt_int_bits"       -> Some(Type_Bits(num))
    | "LSL"                -> Some(Type_Bits(num))
    | "LSR"                -> Some(Type_Bits(num))
    | "ASR"                -> Some(Type_Bits(num))
    | "cvt_bits_uint"      -> Some(type_integer)
    | "cvt_bits_sint"      -> Some(type_integer)
    | "eq_bits"            -> Some(type_bool)
    | "ne_bits"            -> Some(type_bool)
    | "sle_bits"           -> Some(type_bool)
    | "slt_bits"           -> Some(type_bool)
    | _ -> None
    end
  | Expr_TApply((FIdent(name, _) | Ident(name)), [Expr_LitInt(v1) as num1; Expr_LitInt(v2) as num2], _) -> begin
    (* These are... dubious. None appear in value.ml, so they're all based on what "looks correct". *)
    match name with
    | "ZeroExtend"         -> Some(Type_Bits(num2))
    | "SignExtend"         -> Some(Type_Bits(num2))
    | "lsl_bits"           -> Some(Type_Bits(num1))
    | "lsr_bits"           -> Some(Type_Bits(num1))
    | "asl_bits"           -> Some(Type_Bits(num1))
    | "asr_bits"           -> Some(Type_Bits(num1))
    | "append_bits"        ->
      Some(Type_Bits(Expr_LitInt(string_of_int((int_of_string v1) + (int_of_string v2)))))
    | _ -> None
    end
  | _ -> None

(** Remove variables which are unused at the end of the statement list. *)
module RemoveUnused = struct
  let rec remove_unused (globals: IdentSet.t) xs = fst (remove_unused' globals IdentSet.empty xs)

  and remove_unused' globals (used: IdentSet.t) (xs: stmt list): (stmt list * IdentSet.t) =
    List.fold_right (fun stmt (acc, used) ->

      let pass = (acc, used)
      and emit (s: stmt) = (s::acc, IdentSet.union used (fv_stmt s))
      and halt (s: stmt) = ([stmt], IdentSet.empty)
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
        if not (IdentSet.disjoint lvs used) || not (IdentSet.disjoint lvs globals)
          then emit stmt
          else pass

      (* Skip if structure if possible - often seen in decode tests *)
      | Stmt_If(Expr_Var (Ident "TRUE"), tstmts, elsif, fstmts, loc) ->
          let (tstmts',tused) = remove_unused' globals used tstmts in
          (tstmts'@acc,tused)

      | Stmt_If(c, tstmts, elsif, fstmts, loc) ->
        let (tstmts',tused) = remove_unused' globals used tstmts in
        let (fstmts',fused) = remove_unused' globals used fstmts in
        let elsif' = List.map
          (fun (S_Elsif_Cond (c,ss)) ->
            let (b, bused) = remove_unused' globals used ss in
            let bused = IdentSet.union bused (fv_expr c) in
            (S_Elsif_Cond (c,b), bused))
          elsif in
        let used = List.fold_right (fun (_,u) -> IdentSet.union u) elsif' (IdentSet.union tused fused) in
        let used = IdentSet.union used (fv_expr c) in
        (match (tstmts',fstmts',elsif') with
        | [], [], [] -> pass
        | _, _, _ -> (Stmt_If(c, tstmts', List.map fst elsif', fstmts', loc)::acc,used))

      (* Unreachable points *)
      | Stmt_Assert (Expr_Var (Ident "FALSE"), _)
      | Stmt_Throw _ -> halt stmt

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
          | Stmt_ProcReturn _ -> ChangeTo [s]
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

    method! vstmt (s: stmt): stmt list visitAction =
      singletonVisitAction @@ match s with
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

  let is_pos (_,abs) =
    let (_,l) = interval abs in
    Z.geq l Z.zero

  (** Integer variable reads that are successfully converted into
      bitvectors are wrapped in the following function call and
      subsequently unwrapped in a later pass.

      Variable reads that are not wrapped imply a integer variable
      use that this analysis fails to match. To remain compatible,
      these variable reads are subsequently converted back to integers. *)
  let wrapper_ident = FIdent ("StatefulIntToBit_wrapper", 0)

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
        | Some v -> (sym_prim wrapper_ident [] [Exp e], v)
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

    (* Interface only supports zero rounding division at present, force fdiv result to be positive *)
    | Expr_TApply (FIdent ("fdiv_int", 0), [], [x; y]) ->
        let x = force_signed (bv_of_int_expr vars x) in
        let y = force_signed (bv_of_int_expr vars y) in
        assert (is_pos x = is_pos y);
        let w = abs_of_div (snd x) (snd y) in
        let ex = extend w in
        let f = sym_prim (FIdent ("sdiv_bits", 0)) [sym_of_abs w] [ex x; ex y] in
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
        let f = sym_prim (FIdent ("not_bits", 0)) [sym_of_abs w] [ex x] in
        let offset = Val (VBits {v=Z.one; n=width w}) in
        let f = sym_prim (FIdent ("add_bits", 0)) [sym_of_abs w] [f; offset] in
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
            (* in worst case, could shift upper bound on y, adding y bits *)
            let size = width (snd x) + (Z.to_int (Z.max u Z.zero)) in
            let abs = if signed (snd x) then abs_of_width size else abs_of_uwidth size in
            let ex = extend abs in
            let f = sym_prim (FIdent ("lsl_bits", 0)) [sym_of_int size; sym_of_abs (snd y)] [ex x;fst y] in
            (f,abs)
        )

    (* TODO: Over-approximate range on result, could be a little closer *)
    | Expr_TApply (FIdent ("shr_int", 0), [], [x; y]) ->
        let x = force_signed (bv_of_int_expr vars x) in
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
          let (e,a) = force_signed (bv_of_int_expr vars e) in
          let w = int_of_expr t in
          if w < width a then
            sym_expr @@ sym_slice Unknown e 0 w
          else
            let abs = (int_of_expr t,true,(Z.zero,Z.zero)) in
            sym_expr @@ extend abs (e,a)

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

      (* Translation from enum to bit *)
      | Expr_TApply (FIdent ("eq_enum", n), [], [x;y]) when n > 0 ->
          let x = bv_of_int_expr vars x in
          let y = bv_of_int_expr vars y in
          let w = merge_abs (snd x) (snd y) in
          let ex = extend w in
          (sym_expr @@ sym_prim (FIdent ("eq_bits", 0)) [sym_of_abs w] [ex x; ex y])

      | Expr_TApply (FIdent ("ne_enum", n), [], [x;y]) when n > 0 ->
          let x = bv_of_int_expr vars x in
          let y = bv_of_int_expr vars y in
          let w = merge_abs (snd x) (snd y) in
          let ex = extend w in
          (sym_expr @@ sym_prim (FIdent ("ne_bits", 0)) [sym_of_abs w] [ex x; ex y])

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
      ChangeDoChildrenPost(e', fun e -> e)
  end

  (** Cleanup pass to remove wrapper and introduce necessary bit->int conversions *)
  class cleanup (vars) = object (self)
    inherit Asl_visitor.nopAslVisitor
    method! vexpr e =
      match e with
      | Expr_TApply (f, [], [e]) when f = wrapper_ident -> ChangeTo e
      | Expr_Var v ->
          (match Bindings.find_opt v vars with
          | Some w ->
              (*Printf.printf "transform_int_expr: Found root var: %s\n" (match v with Ident s -> s | _ -> "");*)
              let prim = if signed w then "cvt_bits_int" else "cvt_bits_uint" in
              ChangeTo (expr_prim' prim [expr_of_abs w] [e])
          | None -> SkipChildren)
      | _ -> DoChildren
  end

  (** Get a variable's abstract rep with a default initial value *)
  let get_default (v: ident) (w: int option) ((_,vars): state): abs =
    match w, Bindings.find_opt v vars with
    | Some w, _ -> abs_of_uwidth w
    | _, Some (a,b,_) -> (a,b,(Z.zero,Z.zero))
    | _, _ -> abs_of_const Z.zero

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

  (* Identify variable types to track, possibly with a minimum initial width for enums *)
  let capture_type enum_types ty: int option option =
    if ty = type_integer then Some None
    else match ty with
    | Type_Constructor i ->
        (match enum_types i with
        | Some w -> Some (Some w)
        | None -> None)
    | _ -> None

  (** Statement list walk to establish variable widths and visit all expressions *)
  (*
     TODO: This won't respect local scopes within If stmts
  *)
  let rec walk enum_types changed (vars: abs Bindings.t) (s: stmt list): (state * stmt list) =
    List.fold_left (fun (st,acc) stmt ->
      let v = new transform_int_expr st in
      match stmt with
      | Stmt_If (e, tstmts, [], fstmts, loc) -> (* Walk the If structure *)
          let e = visit_expr v e in
          let (changed,vars) = st in
          let (t,tstmts) = walk enum_types changed vars tstmts in
          let (f,fstmts) = walk enum_types changed vars fstmts in
          (merge t f,acc@[Stmt_If(e, tstmts, [], fstmts, loc)])

      | _ -> (* Otherwise, we have no statement nesting *)
        let stmt = Asl_visitor.visit_stmt_single v stmt in
        let (st,stmt) = (match stmt with

        (* Match integer writes *)
        | Stmt_VarDeclsNoInit(t, [v], loc) ->
            (match capture_type enum_types t with
            | Some w ->
                let lhs = get_default v w st in
                let e = Stmt_VarDeclsNoInit (type_bits (string_of_int (width lhs)), [v], loc) in
                let st = assign v lhs st in
                (st,e)
            | None -> (st,stmt))
        | Stmt_ConstDecl(t, v, e, loc) ->
            (match capture_type enum_types t with
            | Some w ->
                let lhs = get_default v w st in
                let rhs = bv_of_int_expr st e in
                let w = merge_abs lhs (snd rhs) in
                let s = sym_expr (extend w rhs) in
                let s = Stmt_ConstDecl (type_bits (string_of_int (width w)), v, s, loc) in
                let st = assign v w st in
                (st,s)
            | None -> (st,stmt))
        | Stmt_VarDecl(t, v, e, loc) ->
            (match capture_type enum_types t with
            | Some w ->
                let lhs = get_default v w st in
                let rhs = bv_of_int_expr st e in
                let w = merge_abs lhs (snd rhs) in
                let s = sym_expr (extend w rhs) in
                let s = Stmt_VarDecl (type_bits (string_of_int (width w)), v, s, loc) in
                let st = assign v w st in
                (st,s)
            | None -> (st,stmt))
        | Stmt_Assign(LExpr_Var(v), e, loc) when tracked v st ->
            let lhs = get_default v None st in
            let rhs = bv_of_int_expr st e in
            let w = merge_abs lhs (snd rhs) in
            let s = sym_expr (extend w rhs) in
            let s = Stmt_Assign (LExpr_Var(v), s, loc) in
            let st = assign v w st in
            (st,s)

        (* Ignore all other stmts *)
        | Stmt_VarDeclsNoInit _
        | Stmt_Assign _
        | Stmt_Assert _
        | Stmt_TCall _ -> (st,stmt)
        | _ -> failwith "walk: invalid IR") in
        (st,acc@[stmt])
    ) ((changed,vars),[]) s

  let rec fixedPoint (enum_types: ident -> int option) (vars: abs Bindings.t) (s: stmt list): stmt list =
    let ((changed,vars),res) = walk enum_types false vars s in
    if changed then fixedPoint enum_types vars s
    else Asl_visitor.visit_stmts (new cleanup vars) res

  let run (enum_types: ident -> int option) (s: stmt list): stmt list =
    fixedPoint enum_types Bindings.empty s

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
      | Some (Type_Register (wd, _)) -> int_of_string wd
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

      | Stmt_TCall (FIdent("AtomicStart", 0), _, _, _)
      | Stmt_TCall (FIdent("AtomicEnd", 0), _, _, _) ->
          (acc@[stmt],removeMemory copies)

      | _ ->
          (* Over-approximate all other situations for soundness *)
          if debug_cp then Printf.printf "Over-approx: %s\n" (pp_stmt stmt);
          (acc@[stmt],Bindings.empty))
    ([], copies) xs

  let copyProp (xs: stmt list): stmt list =
    let (acc, _) = copyProp' xs Bindings.empty in
    acc

end

module RedundantSlice = struct

  let non_const e =
    match  e with
    | Expr_LitInt _ -> false
    | Expr_LitHex _ -> false
    | _ -> true

  let option_or x y =
    match x with
    | Some x' -> Some x'
    | None -> y

  let width_of_slice (slice : slice) : int =
    match slice with
    | Slice_LoWd (lo, wd) -> int_of_expr wd
    | Slice_HiLo (hi, lo) -> int_of_expr hi - int_of_expr lo + 1
    | Slice_Single _ -> 1

  let width_of_slices slices = List.fold_left (+) 0 (List.map width_of_slice slices)

  let bits_type_of_reg_type = function
    | Type_Register (wd, _) -> Type_Bits (Expr_LitInt wd)
    | x -> x

  type ty_option = Just of ty | Clobbered

  class expression_walk (vartypes: ty Bindings.t) = object (self)
    inherit Asl_visitor.nopAslVisitor

    (** map of variable name to type.
      a value of "Clobbered" means that variable is declared multiple times with different types
      and we should not remove any of its slices. *)
    val mutable lvartypes : ty_option Bindings.t = Bindings.empty;

    method update_lvar_types (s: stmt): unit =
      match s with
      | Stmt_VarDecl(ty,id,_,l)
      | Stmt_ConstDecl(ty,id,_,l) ->
        (match Bindings.find_opt id lvartypes with
        | Some (Just ty') -> if ty = ty' then () else lvartypes <- Bindings.add id (Clobbered) lvartypes
        | Some (Clobbered) -> ()
        | None -> lvartypes <- Bindings.add id (Just ty) lvartypes)
      | Stmt_VarDeclsNoInit(ty,ids,l) ->
        List.iter (fun id -> self#update_lvar_types (Stmt_VarDecl(ty,id,Expr_LitInt("ignored"),l))) ids
      | _ -> ()

    method var_type (id: ident): ty option =
      Option.map bits_type_of_reg_type
        (match Bindings.find_opt id lvartypes with
        | Some (Just x) -> Some x
        | _ -> Bindings.find_opt id vartypes)

    method var_type' (e: expr): ty option =
      match e with
      | Expr_Var id -> self#var_type id
      | _ -> None

    method array_val_type (id: ident): ty option =
      match self#var_type id with
      | Some (Type_Array(_ix,ty)) -> Some ty
      | _ -> None

    method! vstmt (s: stmt): stmt list visitAction =
      singletonVisitAction @@ ChangeDoChildrenPost(s, fun s -> self#update_lvar_types s; s)

    method! vexpr (e: expr): expr visitAction =
      ChangeDoChildrenPost(e, fun e ->
      match e with
      (* Last chance to convert dynamic slices into shift & static slice *)
      | Expr_Slices(x, [Slice_LoWd(l,w)]) when non_const l ->
          (match option_or (infer_type x) (self#var_type' x) with
          | Some (Type_Bits xw) ->
              let e = Expr_TApply (FIdent ("LSR", 0), [xw], [x; l]) in
              Expr_Slices(e, [Slice_LoWd (Expr_LitInt "0", w)])
          | _ -> e)
      | Expr_Slices(e', [Slice_LoWd (Expr_LitInt "0", wd)]) ->
          let try_match (opt: ty option): expr =
            match opt with
            | Some(Type_Bits(num)) when num = wd -> e'
            | _ -> e
          in
          (match e' with
          (* note: no fall-through from var_type case to infer_type case,
             but infer_type only works for builtins anyway. *)
          | Expr_Var id -> try_match (self#var_type id)
          | Expr_Array (Expr_Var id, _) -> try_match (self#array_val_type id)
          | _ -> try_match (infer_type e'))
      | _ -> e)
  end

  let do_transform (vartypes: ty Bindings.t) (xs: stmt list): stmt list =
    Asl_visitor.visit_stmts (new expression_walk(vartypes)) xs

end


module CommonSubExprElim = struct
  (* Basic common sub-expression elimination.
     (Theoretical) Pitfalls:
     - Type inference of our factorised subexpressions isn't great. See large match statement in infer_cse_expr_type
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
      | Expr_TApply(f,_,_) when List.mem f pure_prims ->
          (match infer_type e with
          | Some (Type_Bits _) ->
              if (List.mem e cand_exprs) && not (List.mem e exprs) then
                exprs <- e :: exprs
              else cand_exprs <- e :: cand_exprs;
          | _ -> ())
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

    method! vstmt (s: stmt): stmt list visitAction =
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
    match infer_type e with
    | Some t -> t
    | None -> raise (CSEError ("Can't infer type of strange expr: " ^ (pp_expr e)))

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
        (* Do replacement in our remaining eliminate-able expressions
           to ensure that they will continue to match correctly *)
        add_exprs_num (insert_into_stmts xs new_stmt) (visit_exprs repl tail) (id+1)
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

(* A brute force match for total value mappings, implemented as a series of chained ifs *)
module CaseSimp = struct
  module StringMap = Map.Make(String);;

  (* Match a 'X = BV_CONSTANT' comparison, returning X and BV_CONSTANT *)
  let valid_guard e =
    match e with
    | Expr_TApply (FIdent ("eq_bits", 0), [Expr_LitInt w], [x; Expr_LitBits b]) ->
        Some (int_of_string w, x, b)
    | _ -> None

  (* Match a 'R := BV_CONSTANT' statement, returning R and BV_CONSTANT *)
  let valid_body b =
    match b with
    | Stmt_Assign (LExpr_Var r, Expr_LitBits c, _) -> Some(r, c)
    | _ -> None

  (* Match a chain of 'if X = BV_CONSTANT then R := BV_CONSTANT else if ... else assert FALSE'
     given specific X and R expressions, returning a map from test values to assigned values *)
  let rec match_inner stmt x r =
    match stmt with
    | Stmt_If (e, [c], [], [f], _) ->
        (match valid_guard e, valid_body c, match_inner f x r with
        | Some (w, x', b), Some (r', c), Some res when x' = x && r = r' -> Some (StringMap.add b c res)
        | _ -> None)
    | Stmt_Assert (Expr_Var(Ident "FALSE"), _) -> Some StringMap.empty
    | _ -> None

  (* Match a chain of 'if X = BV_CONSTANT then R := BV_CONSTANT else if ... else assert FALSE',
     returning X, R and a map from test values to assigned values *)
  let match_outer stmt =
    match stmt with
    | Stmt_If (e, [t], [], [f], loc) ->
        (match valid_guard e, valid_body t with
        | Some (w, x, b), Some (r, c) ->
            (match match_inner f x r with
            | Some res -> Some (x, r, w, loc, StringMap.add b c res)
            | _ -> None)
        | _ -> None)
    | _ -> None

  (* Mapping is total if there is an entry for all possible bv values *)
  let is_total w res = Z.to_int (Z.shift_left Z.one w) = (StringMap.cardinal res)

  (* Guesses for the possible mapping from key to value. This is incredibly dumb. *)
  let fn_guess = [
    (fun x y -> x = y),
    (fun r x _ loc -> Stmt_Assign(LExpr_Var r, x, loc));
    (fun x y -> "0" ^ x = y),
    (fun r x w loc ->
      let nw = expr_of_int (w + 1) in
      Stmt_Assign(LExpr_Var r, expr_prim' "ZeroExtend" [expr_of_int w; nw] [x; nw], loc));
  ]

  class visit_if = object
    inherit Asl_visitor.nopAslVisitor

    (* Assumes x is pure, as it is referenced within a branch condition *)
    method! vstmt (s: stmt): stmt list visitAction =
      match match_outer s with
      | Some (x, r, w, loc, res) when is_total w res ->
          (match List.find_opt (fun (test,_) -> StringMap.for_all test res) fn_guess with
          | Some (_,fn) -> ChangeTo [fn r x w loc]
          | _ -> DoChildren)
      | _ -> DoChildren

  end

  let do_transform (xs: stmt list): stmt list =
    let stmt_visitor = new visit_if in
    let xs = visit_stmts stmt_visitor xs in
    xs
end

(* Rewrite expressions with temporary dynamic width bitvectors into equivalent versions with only static bitvectors *)
module RemoveTempBVs = struct

  class expr_walker debug = object
    inherit Asl_visitor.nopAslVisitor
    method !vslice s =
      match s with
      | Slice_HiLo(Expr_TApply(FIdent("add_int", 0), [], [a;Expr_LitInt b]),lo) when a = lo ->
          ChangeTo( Slice_LoWd(lo, Expr_LitInt (string_of_int (int_of_string b + 1))) )
      | _ -> DoChildren
    method !vexpr e =
      match e with
      | Expr_TApply (FIdent("ZeroExtend", 0), [m;Expr_LitInt n], (Expr_TApply(FIdent("Ones", 0), [zw], ones)::xs)) ->
          let ne = Expr_TApply (FIdent("LSR", 0), [Expr_LitInt n], [Expr_TApply(FIdent("Ones", 0), [Expr_LitInt n], [Expr_LitInt n]);
            Expr_TApply (FIdent ("sub_int", 0), [], [Expr_LitInt n; m])]) in
          if debug then Printf.printf "RemoveTempBVs: Changing '%s' to '%s'\n" (pp_expr e) (pp_expr ne);
          ChangeDoChildrenPost(ne, fun e -> e)
      | _ -> DoChildren
  end

  let do_transform debug (xs: stmt list): stmt list =
    let visitor = new expr_walker debug in
    visit_stmts visitor xs

end

module RemoveRegisters = struct

  class type_walker = object
    inherit Asl_visitor.nopAslVisitor
    method !vtype t =
      match t with
      | Type_Register(w,_) -> ChangeTo (Type_Bits (Expr_LitInt w))
      | _ -> DoChildren
  end

  let run =
    let v = new type_walker in
    visit_stmts v

end


module type ScopedBindings = sig  
    type 'elt t = 'elt Bindings.t Stack.t

    val push_scope : 'elt t  -> unit -> unit 
    val pop_scope : 'elt t  -> unit -> unit 
    val add_bind : 'elt t  -> ident -> 'elt -> unit 
    val find_binding : 'elt t -> ident -> 'elt option 
    val current_scope_bindings : 'elt t -> 'elt Bindings.t
    val init: unit -> 'elt t  
end

module ScopedBindings : ScopedBindings = struct 
  type 'elt t = 'elt Bindings.t Stack.t
  let push_scope (b:'elt t) (_:unit) : unit = Stack.push (Bindings.empty) b 
  let pop_scope (b:'elt t) (_:unit) : unit = Stack.pop_opt b |> ignore 
  let add_bind (b:'elt t) k v : unit = Stack.push (Bindings.add k v (Stack.pop b)) b 
  let find_binding (b:'elt t) (i) : 'a option = Seq.find_map (fun s -> Bindings.find_opt i s) (Stack.to_seq b)
  let init (u:unit) : 'elt t = let s = Stack.create () in Stack.push (Bindings.empty) s; s

  let current_scope_bindings (b:'elt t) : 'elt Bindings.t = 
    let keyset c = IdentSet.of_list (Bindings.bindings c |> List.map fst) in
    let keysdiff a b = IdentSet.diff (keyset a) (keyset b) in
    let join bas bbs = Bindings.add_seq (Seq.map (fun i -> i, Bindings.find i bbs) (IdentSet.to_seq (keysdiff bbs bas))) bas in
    List.fold_left join Bindings.empty (List.of_seq (Stack.to_seq b))
end

module FixRedefinitions = struct
  type var_t = {name: ident ; index: int}

  let ident_for_v (e: var_t) : ident =
    if e.index = 0 then e.name else
    match e.name with
    | Ident s -> Ident (s ^ "_" ^ (string_of_int e.index))
    | FIdent (s, i) -> FIdent ((s ^ "_" ^ (string_of_int e.index), i))

  open ScopedBindings

  class redef_renamer (globals) = object(this)
    inherit Asl_visitor.nopAslVisitor

    val mutable seen = Bindings.empty
    val scoped_bindings : var_t ScopedBindings.t =
      let s = Stack.create () in
      Stack.push (Bindings.empty) s ; s

    method push_scope (_:unit) : unit = push_scope scoped_bindings ()
    method pop_scope (_:unit) : unit = pop_scope scoped_bindings () 
    method add_bind (n: var_t) : unit = add_bind scoped_bindings n.name n 
    method existing_binding (i: ident) : var_t option = find_binding scoped_bindings i

    method incr_binding (i: ident) : var_t =
      let v = this#existing_binding i in
      match v with
      | Some b -> {b with index = b.index + 1}
      | None -> {name=i; index=0}

    method! vstmt s =
      singletonVisitAction @@ match s with
        | Stmt_VarDeclsNoInit(ty, vs, loc) ->
            let ns = List.map this#incr_binding vs in
            List.iter this#add_bind ns; DoChildren
        | Stmt_VarDecl(ty, v, i, loc) ->
            let b = this#incr_binding v in
            this#add_bind b; DoChildren
        | Stmt_ConstDecl(ty, v, i, loc) ->
            let b = this#incr_binding v in
            this#add_bind b; DoChildren
        | Stmt_If (c, t, els, e, loc) ->
            let c'   = visit_expr this c in
            this#push_scope () ;
            let t'   = visit_stmts this t in
            this#pop_scope (); this#push_scope () ;
            let els' = mapNoCopy (visit_s_elsif this ) els in
            this#pop_scope (); this#push_scope () ;
            let e'   = visit_stmts this e in
            this#pop_scope ();
            ChangeTo (Stmt_If (c', t', els', e', loc))
        (* Statements with child scopes that shouldn't appear towards the end of transform pipeline *)
        | Stmt_Case _ -> failwith "(FixRedefinitions) case not expected"
        | Stmt_For _ -> failwith "(FixRedefinitions) for not expected"
        | Stmt_While _ -> failwith "(FixRedefinitions) while not expected"
        | Stmt_Repeat _ -> failwith "(FixRedefinitions) repeat not expected"
        | Stmt_Try _ -> failwith "(FixRedefinitions) try not expected"
        | _ -> DoChildren

    method! vlvar e =
       (match (this#existing_binding e) with
          | Some e -> ChangeTo (ident_for_v e)
          | None -> SkipChildren)

    method! vvar e =
       (match (this#existing_binding e) with
          | Some e -> ChangeTo (ident_for_v e)
          | None -> SkipChildren)
    end

  let run (g: IdentSet.t) (s:stmt list) : stmt list =
    let v = new redef_renamer g in
    visit_stmts v s
end

