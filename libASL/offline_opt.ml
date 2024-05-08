open Asl_utils
open Asl_ast

(* Utility functions to match runtime expressions *)
let is_memory_load f =
  f = FIdent ("gen_Mem.read", 0)
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

let is_merge_target f2 =
  f2 = Offline_transform.rt_merge_branch

let is_gen_call f =
  let prefix = "gen_" in
  match f with
  | FIdent(f, _) when String.starts_with ~prefix f -> true
  | _ -> false

let is_pure_expr f =
  let prefix = "gen_" in
  match f with
  | FIdent(f, 0) when String.starts_with ~prefix f ->
      let f' = String.sub f 4 (String.length f - 4) in
      List.mem f' Offline_transform.pure_prims
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
    | e -> (Printf.printf "Unknown runtime expression: %s\n"  (pp_expr e)); DoChildren
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
    | Stmt_ConstDecl(t, v, Expr_TApply(f, [], [c]), loc) when is_branch f ->
        (* Collect reads and process them all *)
        let deps = get_deps c in
        let st = read_vars deps st in
        (* Push the merge point *)
        push_context v st

    (* Context switch *)
    | Stmt_TCall(f, [], [Expr_TApply(f2, [], [Expr_Var i])], loc) when is_context_switch f && is_merge_target f2 ->
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

  (* To change this, you'd need to know :
      - The condition under which its safe to copy prop
      - The current reachability

     If you can't establish you are guarded, implies you need to introduce a branch.
     The branch will have the outcomes of both exp reduction and maintaining the current temp.
     Then need to specialise everything downstream for this point based on this introduced branch.

     This means you need to pull the condition out to the front.
     Which means its needs to be fully reduced and in terms of enc.
     BDD approach gives us this flexibility, every single condition in the program in terms of original enc.
     Relatively simple to reduce from that point: eliminate guards based on reachability, etc.

     You can implement constant-prop and dead code in a similar fashion, as long as your notions of conditional
     use / redefinition / loss of constant precision is purely in terms of the original enc.
   *)
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
          ChangeDoChildrenPost([Stmt_VarDeclsNoInit(Offline_transform.rt_expr_ty, [v], loc)], fun e -> e)
      (* Transform stores into assigns *)
      | Stmt_TCall(f, [], [Expr_Var v; e], loc) when is_var_store f && candidate_var v st ->
          ChangeDoChildrenPost([Stmt_Assign(LExpr_Var v, e, loc)], fun e -> e)
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

module DeadContextSwitch = struct
  (* Backwards walk to reduce consecutive context switches.
     Could be extended to any context switches with no rt gen operations between,
     but this pattern doesn't seem to show up. *)

  let rec walk_stmts s dead =
    List.fold_right (fun s (acc,dead) ->
      match s with
      | Stmt_TCall (f, _, _, _) when is_context_switch f && dead -> (acc,dead)
      | Stmt_TCall (f, _, _, _) when is_context_switch f -> (s::acc,true)
      | Stmt_If(c, t, [], f, loc) ->
          let (t,dead) = walk_stmts t dead in
          let (f,dead') = walk_stmts f dead in
          (Stmt_If(c, t, [], f, loc)::acc, dead && dead')
      | _ -> (s::acc,false)
    ) s ([],dead)

  let run fn body = let (s,_) =  walk_stmts body false in s
end


module RtCopyProp = struct
  type clas =
    Declared |
    Defined of IdentSet.t |
    Clobbered |  (* means there is a clobber on at least one branch *)
    Essential (* not a candidate: there is a read following a clobber or dependent on rt branch *)

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
    ctx : (ident * MLBDD.t) list;
    (* maps idents to the condution under which they are clobbered *)
    cond_clobbered: (Transforms.BDDSimp.abs) Bindings.t; (* ident -> clobber condition (any dep updated) *)
    (* maps idents to the condition under which they are read after clobbering *)
    cond_read_after_clobber: (Transforms.BDDSimp.abs) Bindings.t; (* ident -> clobber condition (any dep updated) *)
    (* not used; stores dep sets for bindings (and the def reachability) *)
    cond_dep: (Transforms.BDDSimp.abs * IdentSet.t) Bindings.t;  (* binding -> condition * deps *)
    (**)
    bdd: Transforms.BDDSimp.state;
  }

  let set_var v k st =
    let var_clas = Bindings.add v k st.var_clas in
    let _  = match k with 
      | Declared -> ()
      | Defined x -> ()
      | Clobbered  -> ()
      | Essential -> ()
    in
    { st with var_clas }


  let cond_merge al bl =  Bindings.merge (fun i a b  -> match a,b  with 
      | Some a, Some b -> Some (Transforms.BDDSimp.or_bits a b)
      | Some a, _ -> Some a 
      | _ , Some b -> Some b
      | _ -> None) al bl

  let add_cond i c bs = Bindings.add i (match (Bindings.find_opt i bs) with 
    | Some x -> (Transforms.BDDSimp.or_bits c x)
    | None -> c
  ) bs

  let add_conds is c bs = cond_merge bs (Seq.map (fun i -> i, c) is |>  Bindings.of_seq)

  let clobber_var v st =
    let var_clas = Bindings.map (fun c -> match c with Defined ids when IdentSet.mem v ids -> Clobbered | _ -> c) st.var_clas in
    (* TODO: should clobbering transitive?*)
    let deps = Bindings.filter_map (fun i c -> match c with Defined ids when IdentSet.mem v ids -> 
      Some (Transforms.BDDSimp.Val [st.bdd.ctx]) | _ -> None) st.var_clas in
    let cond_clobbered = cond_merge st.cond_clobbered deps in
    { st with var_clas ; cond_clobbered }

  let get_var v st = Bindings.find_opt v st.var_clas

  let merge_st cond a b =
    let ctx = a.ctx in
    let merged_bdd a b = Bindings.merge (fun (k:ident) (a:Transforms.BDDSimp.abs option) (b:Transforms.BDDSimp.abs option) -> match a,b with 
      | Some a, Some b -> Some (Transforms.BDDSimp.join_abs cond a b)
      | Some a, None -> Some a
      | None, Some a -> Some a
      | None, None -> None)  a b in
    let cond_clobbered = merged_bdd a.cond_clobbered b.cond_clobbered in
    let cond_read_after_clobber = merged_bdd a.cond_read_after_clobber b.cond_read_after_clobber in
    let cond_dep = Bindings.merge (fun k a b -> match a,b with 
        | Some (isa, a), Some (isb, b) -> Some (Transforms.BDDSimp.join_abs cond isa isb, IdentSet.union a b)
        | Some a, None -> Some a
        | None, Some a -> Some a
        | _ -> None
    ) a.cond_dep b.cond_dep in
    let var_clas = Bindings.merge (fun k a b ->
      match a, b with
      | Some a, Some b -> Some (merge_clas a b)
      | Some a, None
      | None, Some a -> Some a
      | None, None -> None) a.var_clas b.var_clas in
    let bdd = Transforms.BDDSimp.join_state cond a.bdd b.bdd in
    { bdd; var_clas ; ctx ; cond_clobbered;  cond_read_after_clobber; cond_dep }
  let init_state reachable = {bdd=Transforms.BDDSimp.init_state reachable; 
    var_clas = Bindings.empty; ctx = []; 
    cond_clobbered = Bindings.empty ; 
    cond_read_after_clobber = Bindings.empty ; 
    cond_dep = Bindings.empty}
  let push_context m st = { st with ctx = m::st.ctx }
  let peek_context st = match st.ctx with x::xs -> x | _ -> invalid_arg "peek_context"
  let pop_context st = let (i,c),tl = (match st.ctx with x::xs -> x,xs | _ -> invalid_arg "pop_context") in
    { st with ctx = tl ; bdd = {st.bdd with ctx = c} }
  let has_context st = List.length st.ctx > 0

  let decl_var v st = set_var v Declared st
  let define_var v deps st = 
    let r = set_var v (Defined deps) st in 
    let cond_dep = Bindings.find_opt v st.cond_dep |> 
      Option.map (fun (c,b) -> Transforms.BDDSimp.or_bits c (Transforms.BDDSimp.Val [st.bdd.ctx]), IdentSet.union b deps) |>
    function 
    | Some c -> Bindings.add v c st.cond_dep
    | None -> st.cond_dep
    in
    {r with cond_dep }

  type xform = 
    | Prop 
    | PropCond of Transforms.BDDSimp.abs (* copyprop not allowed , should copyprop *)
    | No

  let read_var v (st,i) =
    match get_var v st with
    (* Reading undeclared generally means a value that is gradually constructed through partial updates *)
    | Some (Declared) ->
        (set_var v Essential st, i)
    (* Reading clobbered implies we cannot reorder *)
    | Some (Clobbered) -> (
        (* record read reachability *)
        let clobbered = (Bindings.find v st.cond_clobbered)  in
        let read_after_clobber = Transforms.BDDSimp.and_bits clobbered (Val [st.bdd.ctx]) in
        let st = {st with cond_read_after_clobber = (add_cond v read_after_clobber st.cond_read_after_clobber)} in
        st, i )
        (*if (Transforms.BDDSimp.is_true clobbered st.bdd) then (set_var v Essential st, i) else st, i) *)
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
    if has_context st then set_var v Essential st (* cannot copy-prop exprs dependent on a run-time branch*)
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
      | e -> (Printf.printf "Unknown runtime expression: %s\n"  (pp_expr e)); DoChildren
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
    let eval_post s st = {st with bdd = Transforms.BDDSimp.eval_stmt Transforms.BDDSimp.nop_transform s st.bdd } in
    match s with
    (* Var decl *)
    | Stmt_ConstDecl(t, v, Expr_TApply(f, [], args), loc) when is_var_decl f ->
        decl_var v st 
      |> eval_post s

    (* Var assign *)
    | Stmt_TCall(f, [], [Expr_Var v; e], loc) when is_var_store f ->
        (* Collect reads and process them all *)
        let deps = get_deps e in
        let st = read_vars deps st in
        (* Clobber anything dependent on v *)
        let st = clobber_var v st in
        (* Update deps for v *)
        update_deps v deps st
      |> eval_post s

    (* Array assign *)
    | Stmt_TCall(f, [], [Expr_Var a; i; e], loc) when is_array_store f ->
        (* Collect reads and process them all *)
        let deps = get_deps e in
        let st = read_vars deps st in
        (* Clobber anything dependent on a *)
        clobber_var a st
      |> eval_post s

    (* Assert *)
    | Stmt_TCall(f, [], [e], loc) when is_assert f ->
        (* Collect reads and process them all *)
        let deps = get_deps e in
        read_vars deps st
      |> eval_post s

    (* LiftTime branch *)
    | Stmt_If(c, t, [], f, loc) ->
        (* merge in the bdds as well *)
        let cond = Transforms.BDDSimp.eval_expr c st.bdd in
        let c = Transforms.BDDSimp.rebuild_expr c cond st.bdd in
        let ncond = Transforms.BDDSimp.not_bool cond in
        let tst:state = walk_stmts t {st with bdd = (Transforms.BDDSimp.restrict_ctx cond {st.bdd with stmts = []})} in
        let fst:state = walk_stmts f {st with bdd = (Transforms.BDDSimp.restrict_ctx ncond {st.bdd with stmts = []})} in
        let st': state  = merge_st cond tst fst in
        let st'= {st' with bdd = Transforms.BDDSimp.writeall st.bdd.stmts st'.bdd} in
        let st' = {st' with bdd = Transforms.BDDSimp.write (Stmt_If (c, tst.bdd.stmts, [], fst.bdd.stmts, loc)) st'.bdd} in
        st'

    (* RunTime branch *)
    | Stmt_ConstDecl(t, v, Expr_TApply(f, [], [c]), loc) when is_branch f ->
        (* Collect reads and process them all *)
        let deps = get_deps c in
        let st = read_vars deps st in
        (* Push the merge point *)
        push_context (v, st.bdd.ctx) st
      |> eval_post s

    (* Context switch *)
    | Stmt_TCall(f, [], [Expr_TApply(f2, [], [Expr_Var i])], loc) when is_context_switch f && is_merge_target f2 ->
        let top = fst (peek_context st) in
        if i = top then ((pop_context st)) else st
      |> eval_post s

    (* Impure effect *)
    | Stmt_TCall(f, _, es, loc) when is_gen_call f ->
        (* Collect reads and process them all *)
        let st = List.fold_right (fun e st ->
          let deps = get_deps e in
          read_vars deps st) es st in
        (* Clobber everything linked to global state *)
        clobber_var impure_ident st
      |> eval_post s

    | v -> eval_post v st

  and walk_stmts s st : state =
    List.fold_left (fun st s -> walk_stmt s st) st s

  let candidate_var v st =
    match get_var v st with
    | Some Essential -> false
    | None -> false
    | _ -> true

  (* To change this, you'd need to know :
      - The condition under which its safe to copy prop
      - The current reachability

     If you can't establish you are guarded, implies you need to introduce a branch.
     The branch will have the outcomes of both exp reduction and maintaining the current temp.
     Then need to specialise everything downstream for this point based on this introduced branch.

     This means you need to pull the condition out to the front.
     Which means its needs to be fully reduced and in terms of enc.
     BDD approach gives us this flexibility, every single condition in the program in terms of original enc.
     Relatively simple to reduce from that point: eliminate guards based on reachability, etc.

     You can implement constant-prop and dead code in a similar fashion, as long as your notions of conditional
     use / redefinition / loss of constant precision is purely in terms of the original enc.

statement s is the only definition of x reaching u on every path from s to u there are no assignments to y 

   *)
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
        ChangeDoChildrenPost([Stmt_VarDeclsNoInit(Offline_transform.rt_expr_ty, [v], loc)], fun e -> e)
      (* Transform stores into assigns *)
      | Stmt_TCall(f, [], [Expr_Var v; e], loc) when is_var_store f && candidate_var v st ->
        ChangeDoChildrenPost([Stmt_Assign(LExpr_Var v, e, loc)], fun e -> e)
      | _ -> DoChildren
  end


  (*
    variable is not clobbered then read
  *)
  let cond_candidate v st rtst = 
    match get_var v st with
    | Some Essential -> No
    | Some Clobbered ->
        let c = Bindings.find_opt v st.cond_read_after_clobber in
        (match c with
          | Some x -> PropCond x
          | None -> No)
    | Some Defined _ -> Prop
    | Some Declared -> No 
    | None -> No 


  let cp_idents = function 
    | Ident c -> Ident (c ^ "_nocopyprop") , Ident (c ^ "_copyprop")
    | _ -> failwith "only copyprop vars"

  type cand = {
    typ: ty
  }

  class cond_copyprop_transform cpst = object(self) 
    inherit Asl_visitor.nopAslVisitor
    val mutable rtst = None

    val mutable candidates : cand Bindings.t = Bindings.empty

    method xf_stmt (x:stmt) (st:Transforms.BDDSimp.state) : stmt list = 
      rtst <- Some st; Asl_visitor.visit_stmt self x 

    method candidate v = (Prop = (cond_candidate v cpst (Option.get rtst)))
    method essential v = (No = (cond_candidate v cpst (Option.get rtst)))

    method! vstmt s = ChangeDoChildrenPost (self#stmt_xform s, fun x -> x)
    method! vexpr e = ChangeDoChildrenPost (self#expr_xform e, fun x -> x)


  (*
    Decl of candidate -> decl of expr ref + decl of tmp (unless its never clobbered)
    Write to candidate -> if !clobbered, write to expr ref, else write to tmp
    Read of candidate -> Wrap whole statement in same test, read from appropriate var
  *)

    (*
    For run-time variables that we have determined we can copyprop, 
    pull them to lift-time variables so they can be conditionally 
    copy-propagated at lift time. 
    *)
    method stmt_xform (s : stmt) : stmt list = 
    let cp_cond c = Transforms.BDDSimp.rebuild_expr (Expr_Var (Ident "TRUE")) (c) (Option.get rtst) in
    match s with 
      (* Transform runtime variable decls into expression decls *)
      | Stmt_ConstDecl(t, v, Expr_TApply(f, [], args), loc) when is_var_decl f  ->
          candidates <- Bindings.add v {typ=t} candidates; 
          (match (cond_candidate v cpst (Option.get rtst)) with 
            (* essential, leave as-is *)
            | No ->  [s] 
              (* move run-time to lift-time *)
            | Prop ->  [Stmt_VarDeclsNoInit (Offline_transform.rt_expr_ty, [v], loc)]  
            | PropCond cond -> let ncp,cp = cp_idents v in
              let c = cp_cond cond in
              let rt_decl = Stmt_If (c, [], [] , [Stmt_Assign (LExpr_Var ncp, Expr_TApply(f, [], args), Unknown)], Unknown) in
              (* lift-time conditionally generates the copy-propagated or non-propagated form *)
              [ 
                Stmt_VarDeclsNoInit (Offline_transform.rt_expr_ty, [ncp], loc);
                Stmt_VarDeclsNoInit (Offline_transform.rt_expr_ty, [cp], loc);
                rt_decl
              ]
          )
      (* Transform stores into assigns *)
      | Stmt_TCall(f, [], [Expr_Var v; e], loc) when is_var_store f  ->
          (match (cond_candidate v cpst (Option.get rtst)) with 
            | No ->  [s]
            | Prop ->  [(Stmt_Assign (LExpr_Var v, e, loc))]
            | PropCond cond -> let nocp,cp = cp_idents v in
              (*
                 - if copy-prop'ed form is reachable then generate a store statement
                 - if non-copyprop'ed form is reachable then generate an assignment statement
              *)
              (* can re-evaluating an expression have side effects? *)

              let gen_store_rt_var = Stmt_TCall(f, [], [Expr_Var nocp; e], loc) in
              let assign_lt_var = Stmt_Assign(LExpr_Var cp, e, loc) in
              (* TODO: could further narrow cases here using bdd*)
              [Stmt_If ( cp_cond cond, [gen_store_rt_var], [], [assign_lt_var], Unknown)]
        )
      | _ -> [s]

    method expr_xform (e:expr) : expr = match e with
      | Expr_TApply(f, [], [Expr_Var v]) when is_var_load f ->
          (match (cond_candidate v cpst (Option.get rtst)) with 
          | No ->  e
          | Prop ->  Expr_Var v
          | PropCond cpcond -> let ncp,cp = cp_idents v  in
            let load = Expr_TApply(f, [], [Expr_Var ncp]) in
            let prop = Expr_Var cp in
            let yescpcond = Transforms.BDDSimp.rebuild_expr (Expr_Var (Ident "TRUE")) cpcond (Option.get rtst) in
            let vt = Bindings.find v candidates in
            (* TODO: might be good to check that yes and no are disjoint here *)
            let e = Expr_If (vt.typ, yescpcond, prop, [] , load) in
          e  
        )
      | _ -> e
  end

  let do_transform reachable copyprop_st stmts = 
    (* apply BDD AI a second time to compare reachability with candidates in analysis pass *)
    let st = Transforms.BDDSimp.init_state reachable in
    let st = Transforms.BDDSimp.set_enc st in
    let st' = Transforms.BDDSimp.eval_stmts (copyprop_st) stmts st in
    st'.stmts

  let run fn reachable  body =
    let st = init_state reachable in
    let st = walk_stmts body st in
    (* Printf.printf "%s : %s\n" (pprint_ident fn) (pp_essential st); *)
    (* Printf.printf "%s : %s\n" (pprint_ident fn) (pp_state st); *)
    let v = new cond_copyprop_transform st in
    do_transform reachable v body

end
