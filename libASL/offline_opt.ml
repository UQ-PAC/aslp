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

module TailCallForm = struct

  (*
  Enforce size threshold of procedures by putting in the form; 
    a + f1 ( b + f2 (c + f3 (d + f4 (return x))))
  *)

  class stmt_counter = object(this)
    inherit Asl_visitor.nopAslVisitor
    val mutable stmt_count: int  = 0
    val mutable expr_count: int  = 0

    method !vstmt s = stmt_count <- stmt_count + 1; DoChildren

    method !vexpr s = expr_count <- expr_count + 1; DoChildren

    method count (s:stmt) : int = stmt_count <- 0; (Asl_visitor.visit_stmt this s) |> ignore; stmt_count

    method expr_count (e:expr) : int = expr_count <- 0; (Asl_visitor.visit_expr this e) |> ignore ; expr_count
    method gexpr_count = expr_count
  end

  type st = {
    const_var_types: (ty * bool) Bindings.t; (* not here means global *)
    live_vars: IdentSet.t;
  }

  let s_count s = (new stmt_counter)#count s 

  let uniq_counter : int ref = ref 0
  let new_index _ : int = uniq_counter := !uniq_counter + 1 ; !uniq_counter
  let new_name pref = match pref with 
    | Ident pref ->  Ident ( pref ^ "_split" ^ (string_of_int (new_index ())))
    | FIdent (pref, _) ->  FIdent ( pref ^ "_split" ^ (string_of_int (new_index ())), 0)
  let new_indexs (b: string) : string = b ^ (string_of_int (new_index ()))

  type fn = {
    name: ident; 
    targs: (ident) list;
    args: (ty * ident) list;
    returning: bool;
    body: stmt list;
  }

    (* backwards walk, accumlate statements in accl until over outline threshold, then replace with a call 
       to a function including accl statements. Assuming rem has body in reverse order 

      rem: function being processed containing reversed statement list, minus statements in accl
      accl: forwards statement list of the previously-visited last n statements of the function rem
      acc: list of functions emitted by analysis
      count: cumulative count of accl statement/expression complexity
      returning: is the current function rem returning a value.
    *)
    let rec walk_stmts threshold (st:st) (rem:fn) (accl:stmt list) (acc:fn list) count returning =
    (* TODO: also outline if statement bodies*)
      match rem.body with 
        | [] -> {rem with body = accl}::acc
        | s::tl -> 
          (let returning = (match s with 
            | Stmt_FunReturn _ -> true
            | _ -> returning)
          in let scount = (s_count s) in
          if (scount > threshold) then (Printf.printf "warn: single statement over outline threshold %d in %s\n" scount (pprint_ident rem.name));
          let count = count + scount  in
          let st' = {st with live_vars = IdentSet.union (fv_stmt s) st.live_vars} in
          if (count > threshold) then 
            let nfname = new_name (rem.name)  in
            let arglist = IdentSet.filter (fun i -> (Bindings.mem i st.const_var_types)) st.live_vars in
            let id_mutable_arg i = (match (i, Bindings.find i st.const_var_types) with 
              | (Ident n, (_, true)) -> Ident (n ^ "_mutable")
              | (i, _) -> i
              )
            in
            let arglist = IdentSet.map id_mutable_arg arglist in
            let arglist = IdentSet.to_seq arglist |> List.of_seq in
            let mutable_decls = Bindings.filter (fun i (t,b) -> b) st.const_var_types in
            let mutable_decls = Bindings.to_seq mutable_decls |> Seq.map (fun (i,(t,b)) -> Stmt_VarDecl (t, i, Expr_Var (id_mutable_arg i), Unknown)) |> List.of_seq in
            let newfun : fn = {name=nfname; targs=[]; args=List.map (fun i -> fst (Bindings.find i st.const_var_types) , i) arglist; 
              returning; 
              body=mutable_decls @ s::accl}
            in
            let argslist = List.map (fun i -> Expr_Var i) arglist in
            let call =  if returning 
              then Stmt_FunReturn (Expr_TApply (newfun.name, [], argslist), Unknown) 
              else Stmt_TCall (newfun.name, [], argslist, Unknown) in
            (walk_stmts threshold st' {rem with body = tl; returning = returning} [s; call] (newfun::acc) 
            (if scount < threshold then scount else 0)
            returning)
          else
            (walk_stmts threshold st' {rem with body = tl; returning = returning} (s::accl) acc count returning)
          )

  let outline threshold name (fn: Eval.fun_sig) : (ident * Eval.fun_sig) list =  
    let f = {name=name; 
      returning=Option.is_some (fnsig_get_rt fn);
      targs=fnsig_get_targs fn;
      args=fnsig_get_typed_args fn; 
      body=List.rev (fnsig_get_body fn);
    } in 
    let mutabledefs = (mutable_locals_of_stmts (fnsig_get_body fn)) in
    let ns =  {const_var_types = mutabledefs ; live_vars = IdentSet.empty} in 
    let upd x y = x  in
    let funcs = walk_stmts threshold ns f [] [] 0 false  in
    List.map (fun f -> f.name, 
      (let fn = fnsig_upd_body (upd (f.body)) fn in
      let fn = fnsig_upd_typed_args (upd f.args) fn in  
      let fn = fnsig_upd_args (upd (List.map snd f.args)) fn in
      let fn = fnsig_upd_targs (upd f.targs) fn in
      fn)
    ) funcs
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
