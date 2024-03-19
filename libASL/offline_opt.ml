open Asl_utils
open Asl_ast

let trim_gen f =
  let prefix = "gen_" in
  match f with
  | FIdent(f, i) when String.starts_with ~prefix f ->
      Some ( FIdent (String.sub f 4 (String.length f - 4), i) )
  | _ -> None

(* Utility functions to match runtime expressions *)
let is_memory_load f =
  match trim_gen f with
  | Some f -> f = Dis_config.mem_read_prim
  | _ -> false
let is_var_load f =
  f = Offline_transform.rt_gen_load
let is_var_store f =
  f = Offline_transform.rt_gen_store
let is_array_load f =
  f = Offline_transform.rt_gen_array_load
let is_array_store f =
  f = Offline_transform.rt_gen_array_store
let is_assert f =
  f = Offline_transform.rt_gen_assert
let is_branch f =
  f = Offline_transform.rt_gen_branch
let is_context_switch f =
  f = Offline_transform.rt_switch_context
let is_lit f =
  f = Offline_transform.rt_gen_bool_lit || f = Offline_transform.rt_gen_int_lit || f = Offline_transform.rt_gen_bit_lit
let is_slice f =
  f = FIdent ("gen_slice", 0)

let is_gen_call f =
  match trim_gen f with
  | Some _ -> true
  | _ -> false

let is_pure_expr f =
  match trim_gen f with
  | Some (FIdent (f, 0)) -> List.mem f Offline_transform.pure_prims
  | _ -> false

let is_var_decl f =
  f = Offline_transform.rt_decl_bv || f = Offline_transform.rt_decl_bool

module CopyProp = struct
  type clas =
    Declared |
    Defined of IdentSet.t |
    Clobbered |
    Essential

  let pp_clas c =
    match c with
    | Declared -> "Declared"
    | Defined ids -> "Defined (" ^ pp_identset ids ^ ")"
    | Clobbered -> "Clobbered"
    | Essential -> "Essential"

  let merge_clas a b =
    match a, b with
    | Declared, Declared -> Declared

    (* Ignore declared? *)
    | Declared, Defined d
    | Defined d, Declared -> Defined d
    | Declared, Clobbered
    | Clobbered, Declared -> Clobbered

    (* Can't drop essential though - needs to hold once set *)
    | Declared, Essential
    | Essential, Declared -> Essential

    (* Union deps, consider essential even if only conditional *)
    | Defined d, Defined d' -> Defined (IdentSet.union d d')
    | Defined _, Clobbered
    | Clobbered, Defined _ -> Clobbered
    | Defined _, Essential
    | Essential, Defined _ -> Essential

    (* *)
    | Clobbered, Clobbered -> Clobbered
    | Clobbered, Essential
    | Essential, Clobbered
    | Essential, Essential -> Essential

  type state = {
    var_clas : clas Bindings.t;
    ctx : ident list;
  }
  let set_var v k st =
    let var_clas = Bindings.add v k st.var_clas in
    { st with var_clas }
  let clobber_var v st =
    let var_clas = Bindings.map (fun c -> match c with Defined ids when IdentSet.mem v ids -> Clobbered | _ -> c) st.var_clas in
    { st with var_clas }

  let get_var v st = Bindings.find_opt v st.var_clas
  let merge_st a b =
    assert (a.ctx = b.ctx);
    let ctx = a.ctx in
    let var_clas = Bindings.merge (fun k a b ->
      match a, b with
      | Some a, Some b -> Some (merge_clas a b)
      | Some a, None
      | None, Some a -> Some a
      | None, None -> None) a.var_clas b.var_clas in
    { var_clas ; ctx }
  let init_state = { var_clas = Bindings.empty; ctx = [] }
  let push_context m st = { st with ctx = m::st.ctx }
  let peek_context st = match st.ctx with x::xs -> x | _ -> invalid_arg "peek_context"
  let pop_context st = { st with ctx = (match st.ctx with x::xs -> xs | _ -> invalid_arg "pop_context") }
  let has_context st = List.length st.ctx > 0

  let decl_var v st = set_var v Declared st
  let define_var v deps st = set_var v (Defined deps) st

  let read_var v (st,i) =
    match get_var v st with
    (* Reading undeclared generally means a value that is gradually constructed through partial updates *)
    | Some (Declared) ->
        (set_var v Essential st, i)
    (* Reading clobbered implies we cannot reorder *)
    | Some (Clobbered) ->
        (set_var v Essential st, i)
    (* Collect ids for transitive walk given a defined variable *)
    | Some (Defined ids) ->
        (st, IdentSet.union i ids)
    | _ -> (st, i)

  let impure_ident = Ident "CopyProp_impure"

  let read_vars (vs: IdentSet.t) (st: state): state =
    let read_set s st = IdentSet.fold read_var s (st,IdentSet.empty) in
    (* If impure is in the readset, the reads are not pure. Clobber any impure dependencies now. *)
    let st = if IdentSet.mem impure_ident vs then clobber_var impure_ident st else st in
    (* Reading variables after they are clobbered shifts them to essential vars *)
    let rec iter delta seen st =
      let (st,deps) = read_set delta st in
      let seen = IdentSet.union seen delta in
      let delta = IdentSet.diff deps seen in
      if IdentSet.cardinal delta = 0 then st
      else iter delta seen st in
    iter vs IdentSet.empty st

  (* TODO: Updating, check if this has some context dependence *)
  let update_deps v deps st =
    if has_context st then set_var v Essential st
    else
      match get_var v st with
      | Some (Declared) ->
          set_var v (Defined deps) st
      | Some (Defined d') ->
          set_var v (Defined (IdentSet.union deps d')) st
      | _ -> st

  class deps_walker = object (self)
    inherit Asl_visitor.nopAslVisitor
    val mutable deps = IdentSet.empty

    method add_dep i = deps <- IdentSet.add i deps
    method get_deps = deps

    method! vexpr = function
      | Expr_TApply (f, _, _) when is_lit f ->
          SkipChildren
      | Expr_TApply (f, [], [Expr_Var v]) when is_var_load f ->
          self#add_dep v;
          SkipChildren
      | Expr_TApply (f, [], [e;_;_]) when is_slice f ->
          let _ = self#vexpr e in
          SkipChildren
      | Expr_TApply (f, tes, es) when is_pure_expr f ->
          let _ = List.map (self#vexpr) es in
          SkipChildren
      | Expr_TApply (f, [], [Expr_Var a;i]) when is_array_load f ->
          self#add_dep a;
          SkipChildren
      | Expr_TApply(f, _, es) when is_gen_call f ->
          self#add_dep impure_ident;
          let _ = List.map (self#vexpr) es in
          SkipChildren
      | e -> failwith @@ "Unknown runtime expression: " ^ (pp_expr e)
  end

  let get_deps e =
    let v = new deps_walker in
    let _ = Asl_visitor.visit_expr v e in
    v#get_deps

  let pp_state st =
    pp_bindings pp_clas st.var_clas

  let pp_essential st =
    pp_bindings pp_clas (Bindings.filter (fun f v -> v = Essential) st.var_clas)

  let rec walk_stmt s st =
    match s with
    (* Var decl *)
    | Stmt_ConstDecl(t, v, Expr_TApply(f, [], args), loc) when is_var_decl f ->
        decl_var v st

    (* Var assign *)
    | Stmt_TCall(f, [], [Expr_Var v; e], loc) when is_var_store f ->
        (* Collect reads and process them all *)
        let deps = get_deps e in
        let st = read_vars deps st in
        (* Clobber anything dependent on v *)
        let st = clobber_var v st in
        (* Update deps for v *)
        update_deps v deps st

    (* Array assign *)
    | Stmt_TCall(f, [], [Expr_Var a; i; e], loc) when is_array_store f ->
        (* Collect reads and process them all *)
        let deps = get_deps e in
        let st = read_vars deps st in
        (* Clobber anything dependent on a *)
        clobber_var a st

    (* Assert *)
    | Stmt_TCall(f, [], [e], loc) when is_assert f ->
        (* Collect reads and process them all *)
        let deps = get_deps e in
        read_vars deps st

    (* LiftTime branch *)
    | Stmt_If(c, t, [], f, loc) ->
        let tst = walk_stmts t st in
        let fst = walk_stmts f st in
        merge_st tst fst

    (* RunTime branch *)
    | Stmt_Assign(LExpr_Tuple([LExpr_Var tv; LExpr_Var fv; LExpr_Var mv]), Expr_TApply(f, [], [c]), loc) when is_branch f ->
        (* Collect reads and process them all *)
        let deps = get_deps c in
        let st = read_vars deps st in
        (* Push the merge point *)
        push_context mv st

    (* Context switch *)
    | Stmt_TCall(f, [], [Expr_Var i], loc) when is_context_switch f ->
        let top = peek_context st in
        if i = top then pop_context st else st

    (* Impure effect *)
    | Stmt_TCall(f, _, es, loc) when is_gen_call f ->
        (* Collect reads and process them all *)
        let st = List.fold_right (fun e st ->
          let deps = get_deps e in
          read_vars deps st) es st in
        (* Clobber everything linked to global state *)
        clobber_var impure_ident st

    | _ -> st

  and walk_stmts s st =
    List.fold_left (fun st s -> walk_stmt s st) st s

  let candidate_var v st =
    match get_var v st with
    | Some Essential -> false
    | None -> false
    | _ -> true

  class copyprop_transform st = object
    inherit Asl_visitor.nopAslVisitor
    method! vexpr = function
      (* Transform loads into direct variable accesses *)
      | Expr_TApply(f, [], [Expr_Var v]) when is_var_load f && candidate_var v st ->
          ChangeTo (Expr_Var v)
      | _ -> DoChildren
    method! vstmt = function
      (* Transform runtime variable decls into expression decls *)
      | Stmt_ConstDecl(t, v, Expr_TApply(f, [], args), loc) when is_var_decl f && candidate_var v st ->
          ChangeDoChildrenPost(Stmt_VarDeclsNoInit(Offline_transform.rt_expr_ty, [v], loc), fun e -> e)
      (* Transform stores into assigns *)
      | Stmt_TCall(f, [], [Expr_Var v; e], loc) when is_var_store f && candidate_var v st ->
          ChangeDoChildrenPost(Stmt_Assign(LExpr_Var v, e, loc), fun e -> e)
      | _ -> DoChildren
  end

  let run fn body =
    let st = init_state in
    let st = walk_stmts body st in
    (* Printf.printf "%s : %s\n" (pprint_ident fn) (pp_essential st); *)
    (* Printf.printf "%s : %s\n" (pprint_ident fn) (pp_state st); *)
    let v = new copyprop_transform st in
    Asl_visitor.visit_stmts v body

end
