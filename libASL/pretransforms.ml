open Asl_utils

open AST
open Visitor

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

    method! vdecl (d: declaration): declaration Visitor.visitAction =
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
          let ret = Stmt_FunReturn ((match vars with [x] -> x | _ -> Expr_Tuple vars), loc) in
          let body' = replace_returns body [ret] in

          let rty = match ts with [t] -> t | _ -> Type_Tuple ts in
          let funty = (nm, false, [], [], List.map Asl_visitor.arg_of_sformal args @ [(vty, vnm)], rty) in
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

    method! vstmt (s: stmt) =
      match s with
      | Stmt_Assign (LExpr_Write (setter, targs, args), r, loc) ->
        (match Bindings.find_opt setter ref_params with
        | None -> DoChildren
        | Some (_,ns) ->
          let refs = List.map (List.nth args) ns in
          (* Printf.printf "ref param: %s\n" (pp_expr a); *)

          let les = List.map Symbolic.expr_to_lexpr refs in
          let call = Expr_TApply (setter, targs, args @ [r]) in
          ChangeTo [Stmt_Assign ((match les with [x] -> x | _ -> LExpr_Tuple les), call, loc)]
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
          let assign_write = Stmt_Assign ((match les with [x] -> x | _ -> LExpr_Tuple les), write_call, loc) in

          let x =
            [decl_var; assign_tuple; assign_write]
             in
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


