(****************************************************************
 * ASL dissassembler
 *
 * Copyright Arm Limited (c) 2017-2019
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL dissassembler *)

module PP   = Asl_parser_pp
module AST  = Asl_ast
module TC   = Tcheck

open AST
open Asl_utils
open Value

open Symbolic

let debug_level = ref 0

(** (name, arg, location) tuple for tracing disassembly calls.
    For example: ("dis_expr", "1+1", loc).
*)
type dis_trace = (string * string * l) list

exception DisTrace of dis_trace * exn
exception DisUnsupported of l * string
exception DisInternalError of l * string

let unsupported (loc: l) (msg: string) =
  raise (DisUnsupported (loc, msg))

let internal_error (loc: l) (msg: string) =
  raise (DisInternalError (loc, msg))

let print_dis_trace (trace: dis_trace) =
    String.concat "\n" @@ List.map (fun (f, e, l) ->
        Printf.sprintf "... at %s: %s --> %s" (pp_loc l) f e)
        (trace)

let () = Printexc.register_printer
    (function
    | DisTrace (trace, exn) ->
        let trace' =
            if !debug_level >= 1
            then "\n" ^ print_dis_trace trace
            else ""
        in
        Some (Printexc.to_string exn ^ "\n" ^ trace')
    | DisUnsupported (loc, s) ->
        Some ("DisUnsupported: " ^ pp_loc loc ^ ": " ^ s)
    | DisInternalError (loc, s) ->
        Some ("DisInternalError: " ^ pp_loc loc ^ ": " ^ s)
    | Value.Throw (loc, e) ->
        Some ("LibASL.Value.Throw(" ^ Additional.pp_exc e ^ ") at " ^ pp_loc loc)
    | _ -> None)


(** A variable's stack level and original identifier name.
    The "stack level" is how many scopes deep it is.
    For example, globals are level 0 and this increases
    by 1 for each nested function call.  *)
type var = Var of int * ident
let pp_var (Var (i,id)) = Printf.sprintf "Var(%d,%s)" i (pprint_ident id)
let var_ident (Var (i,id)) =
  match i,id with
  | 0,Ident s -> Ident s (* special case globals with no suffix. *)
  | _,Ident s -> Ident (s ^ "__" ^ string_of_int i)
  | _ -> internal_error Unknown "unexpected resolved variable to function identifier"

let var_ident_no_suffix (Var(_,id)) = id

(** Returns the variable's name without mangling, suitable for
    disassembling then resolving again.

    WARNING: should only be used when variable is in the inner-most scope. *)
let var_expr_no_suffix_in_local_scope (Var(_,id)) = Expr_Var id

(** Returns an expression for the variable with a mangled name,
    suitable for emitting in a generated statement. *)
let var_expr v = EVar (var_ident v)

(** Returns an L-expression for the variable with a mangled name,
    suitable for emitting in a generated statement. *)
let var_lexpr v = LExpr_Var (var_ident v)

(** Returns a sym for the variable with a mangled name,
    suitable for use in a subsequent sym expression.

    WARNING: should only be used when the given variable is
    never re-assigned.
    *)
let var_sym_expr v = Exp (var_expr v)

type sym = Symbolic.value
let pp_sym = Symbolic.pp_value

module LocalEnv = struct
    type t = {
        (* local state, also containing globals at the outer-most level.
           ordered with inner scopes first in list. *)
        (* satisfies invariants that:
           - all values/expressions contained are constant and safe to propagate.
           - a value of VUninitialized indicates that value is unknown.
           - VUninitialized itself is only used for scalar types.
             thus, uninitialized structures must be expanded into structures of uninitialized scalars.
           *)
        locals          : (ty * sym) Bindings.t list;
        returnSymbols   : var list;
        numSymbols      : int;
        indent          : int;
        trace           : dis_trace;
    }

    let pp_value_bindings = Additional.pp_list (pp_bindings pp_value)

    let pp_sym_bindings (bss: (ty * sym) Bindings.t list) =
        Additional.pp_list (pp_bindings (fun (_,e) -> Symbolic.pp_value e)) bss

    let sequence_merge (first: t) (second: t): t =
        {
            first with numSymbols = max first.numSymbols second.numSymbols
        }

    let pp_locals (env: t): string =
        let last = List.length env.locals - 1 in
        let withoutGlobals = List.mapi
            (fun i x -> if i = last then Bindings.empty else x) env.locals in
        Printf.sprintf "locals = %s" (pp_sym_bindings withoutGlobals)
        (* Printf.sprintf "locals = %s" (pp_sym_bindings env.locals) *)

    let getReturnSymbol (loc: l) (env: t): var =
        match env.returnSymbols with
        | [] -> internal_error loc "attempt to return from outside a function"
        | e :: rs -> e

    let addReturnSymbol (e: var) (env: t): t =
        {env with returnSymbols = e :: env.returnSymbols}

    let removeReturnSymbol (env: t): t =
        match env.returnSymbols with
        | [] -> internal_error Unknown "attempt to remove return symbol but no return symbols exist"
        | (s::ss) -> {env with returnSymbols = ss}

    let getNumSymbols (env: t): int =
        env.numSymbols

    let incNumSymbols (env: t): int * t =
        let env' = {env with numSymbols = env.numSymbols + 1} in
        (env'.numSymbols, env')

    let getLocalPrefix (env: t): string =
        string_of_int (List.length env.locals)

    let getLocalName (x: ident) (env: t): ident =
        Ident (pprint_ident x ^ "__" ^ getLocalPrefix env)

    (** Adds a local scoping level within the current level.  *)
    let addLevel (env: t): t =
        {env with locals = (Bindings.empty)::env.locals}

    (** Pops the innermost scoping level.  *)
    let popLevel (env: t): t =
        match env.locals with
        | [] -> internal_error Unknown "attempt to pop local scope level but none exist"
        | (_::ls) -> {env with locals = ls}

    (** Adds a new local variable to the innermost scope. *)
    let addLocalVar (loc: l) (k: ident) (v: sym) (t: ty) (env: t): var * t =
        if !Eval.trace_write then Printf.printf "TRACE: fresh %s = %s\n" (pprint_ident k) (pp_sym v);
        let var = Var (List.length env.locals - 1, k) in
        match env.locals with
        | (bs :: rest) -> var, {env with locals = (Bindings.add k (t,v) bs :: rest)}
        | []        -> internal_error Unknown "attempt to add local var but no local scopes exist"

    let addLocalConst = addLocalVar

    (** Resolves the given identifier within the scopes.
        Returns inner-most scope with a matching variable, due to
        shadowing. *)
    let resolveVar (loc: l) (x: ident) (env: t): var =
        let rec go (bs: (ty * sym) Bindings.t list) =
          match bs with
          | [] -> internal_error loc @@ "cannot resolve undeclared variable: " ^ pprint_ident x ^ "\n\n" ^ pp_locals env
          | b::rest when Bindings.mem x b -> Var (List.length rest,x)
          | _::rest -> go rest
        in
        go (env.locals)

    (** Gets the type and value of a resolved variable. *)
    let getVar (loc: l) (x: var) (env: t): (ty * sym) =
        let Var (i,id) = x in
        let n = List.length env.locals - i - 1 in
        match Bindings.find_opt id (List.nth env.locals n) with
        | Some x -> x
        | None -> internal_error loc @@ "failed to get resolved variable: " ^ pp_var x

    (** Resolves then gets the type and value of a resolved variable. *)
    let resolveGetVar (loc: l) (x: ident) (env: t): (var * (ty * sym)) =
        let var = resolveVar loc x env in
        let (t,v) = getVar loc var env in
        (var, (t,v))

    (** Sets a resolved variable to the given value. *)
    let setVar (loc: l) (x: var) (v: sym) (env: t): t =
        if !Eval.trace_write then Printf.printf "TRACE: write %s = %s\n" (pp_var x) (pp_sym v);
        let Var (i,id) = x in
        let n = List.length env.locals - i - 1 in
        match Bindings.find_opt id (List.nth env.locals n) with
        | Some (t,_) ->
          let locals = Additional.nth_modify (Bindings.add id (t,v)) n env.locals in
          { env with locals }
        | None -> internal_error loc @@ "failed to set resolved variable: " ^ pp_var x

    let resolveSetVar (loc: l) (nm: ident) (x: sym) (env: t): t =
      let var = resolveVar loc nm env in
      setVar loc var x env

end

module DisEnv = struct
    include Rws.Make(struct
        type r = Eval.Env.t
        type w = stmt list
        type s = LocalEnv.t
        let mempty = []
        let mappend = (@)
    end)

    open Let

    let catch (f: 'b -> 'a) (x: 'b): 'a option =
        try Some (f x)
        with EvalError _ -> None

    let getVar (loc: l) (x: ident): (ty * sym) rws =
        let* x = gets (LocalEnv.resolveVar loc x) in
        gets (LocalEnv.getVar loc x)

    let mkUninit (t: ty): value rws =
        let+ env = read in
        try
            Eval.mk_uninitialized Unknown env t
        with
            e -> unsupported Unknown @@
                "mkUninit: failed to evaluate type " ^ pp_type t ^ " due to " ^
                Printexc.to_string e

    let join_locals (l: LocalEnv.t) (r: LocalEnv.t): unit rws =
        assert (l.returnSymbols = r.returnSymbols);
        assert (l.indent = r.indent);
        assert (l.trace = r.trace);

        let merge_bindings l r: (ty * sym) Bindings.t rws =
            Bindings.fold (fun k (t1,v1) bs ->
                match Bindings.find_opt k r with
                | None -> bs
                | Some (t2,v2) ->
                    if t2 <> t1 then
                        unsupported Unknown @@
                        Printf.sprintf "cannot merge locals with different types: %s, %s <> %s."
                            (pprint_ident k) (pp_type t1) (pp_type t2);
                    let+ v =
                        (match v1 = v2 with
                        | false -> let+ v = mkUninit t1 in Val v
                        | true -> pure v1)
                    and+ bs' = bs in
                    Bindings.add k (t1,v) bs')
            l (pure Bindings.empty) in
        let* locals' = traverse2 merge_bindings l.locals r.locals in
        let lenv': LocalEnv.t =
        {
            locals = locals';
            returnSymbols = l.returnSymbols;
            numSymbols = max l.numSymbols r.numSymbols;
            indent = l.indent;
            trace = l.trace;
        }
        in
        put lenv'


    let getFun (loc: l) (x: ident): Abstract_interface.fun_sig option rws =
        reads (catch (fun env -> Eval.Env.getFun loc env x))

    let nextVarName (prefix: string): ident rws =
        let+ num = stateful LocalEnv.incNumSymbols in
        Ident (prefix ^ string_of_int num)

    let indent: string rws =
        let+ i = gets (fun l -> l.indent) in
        let h = i / 2 in
        let s = String.concat "" (List.init h (fun _ -> "\u{2502} \u{250a} ")) in
        if i mod 2 == 0 then
            s ^ ""
        else
            s ^ "\u{2502} "

    let debug (minLevel: int) (s: string): unit rws =
        if !debug_level >= minLevel then
            let+ i = indent in
            let s' = Str.global_replace (Str.regexp "\n") ("\n"^i) s in
            Printf.printf "%s%s\n" i s';
            ()
        else
            unit

    let log (s: string): unit rws =
        debug 1 s

    let warn s = debug 0 ("WARNING: " ^ s)

    let scope (loc: l) (name: string) (arg: string) (pp: 'a -> string) (x: 'a rws): 'a rws =
        (* logging header. looks like: +- dis_expr --> 1 + 1. *)
        log (Printf.sprintf "\u{256d}\u{2500} %s --> %s" name arg) >>

        (* add indentation level for logging. *)
        modify (fun l -> {l with indent = l.indent + 1}) >>
        modify (fun l -> {l with trace = (name,arg,loc)::l.trace}) >>
        let* trace = gets (fun l -> l.trace) in

        (* run computation but obtain state and writer to output in debugging. *)
        let* (result,s',w') = locally (catcherror x) in
        let x' = (match result with
        | Error ((DisTrace _) as e, bt) -> Printexc.raise_with_backtrace e bt
        | Error (exn, bt) -> Printexc.raise_with_backtrace (DisTrace (trace, exn)) bt
        | Ok x' -> x') in
        (* restore state and writer. *)
        write w' >>
        put s' >>
        (* remove indentation level. *)
        modify (fun l -> {l with indent = l.indent - 1}) >>
        modify (fun l -> {l with trace = List.tl l.trace}) >>

        (* logging footer. *)
        log (Printf.sprintf "\u{2570}\u{2500} = %s" (pp x')) >>
        let* () = if !debug_level >= 2
            then log (Printf.sprintf "   %s\n" (LocalEnv.pp_locals s'))
            else unit
        and* () = if !debug_level >= 3
            then traverse_ (fun s -> log ("   " ^ pp_stmt s)) w' >> log ""
            else unit
        in
        pure x'
end

type 'a rws = 'a DisEnv.rws

let (let@) = DisEnv.Let.(let*)
let (and@) = DisEnv.Let.(and*)
let (let+) = DisEnv.Let.(let+)
let (and+) = DisEnv.Let.(and+)

let (>>) = DisEnv.(>>)
let (>>=) = DisEnv.(>>=)

(** Convert value to a simple expression containing that value, so we can
    print it or use it symbolically *)
let to_expr = sym_expr

(** Converts a result_or_simplified to a value.
    Raises an exception if an expression is given, as an expression cannot be casted to a value.
    Requires checking beforehand *)
let to_value (v: sym): value =
    match v with
    | Val v' -> v'
    | Exp _ -> raise (EvalError (Unknown, "Unreachable"))

let is_expr (v: sym): bool =
    match v with
    | Val _ -> false
    | Exp _ -> true

let declare_var (loc: l) (t: ty) (i: ident): var rws =
  let@ env = DisEnv.read in
  let@ uninit = DisEnv.mkUninit t in
  let@ var = DisEnv.stateful
    (LocalEnv.addLocalVar loc i (Val uninit) t) in
  let+ () = DisEnv.write [Stmt_VarDeclsNoInit(t, [var_ident var], loc)] in
  var

let declare_assign_var (loc: l) (t: ty) (i: ident) (x: sym): var rws =
  let@ var = DisEnv.stateful
    (LocalEnv.addLocalVar loc i x t) in
  let+ () = DisEnv.write [Stmt_VarDecl(t, var_ident var, sym_expr x, loc)] in
  var

let declare_fresh_named_var (loc: l) (name: string) (t: ty): var rws =
  let@ res = DisEnv.nextVarName name in
  declare_var loc t res

let assign_var (loc: l) (i: var) (x: sym): unit rws =
  DisEnv.modify (LocalEnv.setVar loc i x) >>
  DisEnv.write [Stmt_Assign(LExpr_Var(var_ident i), sym_expr x, loc)]

let declare_const (loc: l) (t: ty) (i: ident) (x: sym): var rws =
  let@ var = DisEnv.stateful
    (LocalEnv.addLocalConst loc i x t) in
  let+ () = DisEnv.write [Stmt_ConstDecl(t, var_ident var, sym_expr x, loc)] in
  var

let declare_fresh_const (loc: l) (t: ty) (name: string) (x: expr): var rws =
  let@ i = DisEnv.nextVarName name in
  declare_const loc t i (Exp x)

(* Captures the given expression and returns a resolved variable to the captured
   name. *)
let capture_expr loc t x: var rws =
  let@ v = declare_fresh_const loc t "Exp" x in
  let@ uninit = DisEnv.mkUninit t in
  let+ () = DisEnv.modify (LocalEnv.setVar loc v (Val uninit)) in
  v

(* Captures the given expression and returns a sym for the captured name.
   Maintains sym invariant of constant pure expression. *)
let capture_expr_sym loc t x: sym rws =
  let+ v = capture_expr loc t x in
  Exp (var_expr v)

(* Captures the given expression into a variable that can be modified.
   Returns a resolved variable reference to the capture. *)
let capture_expr_mutable loc (t: ty) (x: expr): var rws =
  let@ i = DisEnv.nextVarName "Temp" in
  let@ v = declare_assign_var loc t i (Exp x) in
  let@ uninit = DisEnv.mkUninit t in
  let+ () = DisEnv.modify (LocalEnv.setVar loc v (Val uninit)) in
  v

(** Monadic Utilities *)

(** Coerces sym to value, replacing expressions with uninitialised.
    Correctly expands structures to structures of uninit. *)
let sym_val_or_uninit (t: ty) (x: sym): value rws =
  match x with
  | Val v -> DisEnv.pure v
  | Exp e -> DisEnv.mkUninit t



(** Symbolic implementation of an if statement that returns an expression
 *)
let sym_if (loc: l) (t: ty) (test: sym) (tcase: 'a rws) (fcase: 'b rws): sym rws =
  (match test with
  | (VBool (Left true))  -> tcase
  | (VBool (Left false)) -> fcase
  | (VBool (Right e)) ->
      let@ tmp = declare_fresh_named_var loc "If" t in
      (* Evaluate true branch statements. *)
      let@ (_,tenv,tstmts) = DisEnv.locally
          (let@ x = tcase in let+ () = assign_var loc tmp x in x) in
      (* Propagate incremented counter to env'. *)
      let@ env' = DisEnv.gets (fun env -> LocalEnv.sequence_merge env tenv) in
      (* Execute false branch statements with env'. *)
      let@ (fenv,fstmts) = DisEnv.locally_
          (DisEnv.put env' >> fcase >>= assign_var loc tmp) in
      let@ () = DisEnv.join_locals tenv fenv in
      let+ () = DisEnv.write [Stmt_If(Symbolic.lift_expr loc TC.type_bool e, tstmts, [], fstmts, loc)] in
      (* Construct a value matching the value constructor of the true branch's result. *)
      (* Symbolic.copy_type tval (var_expr tmp) *)
      assert false
  | _ -> failwith ("Split on non-boolean value")
  )

(** Symbolic implementation of an if statement with no return *)
let unit_if (loc: l) (test: sym rws) (tcase: unit rws) (fcase: unit rws): unit rws =
  let@ r = test in
  (match r with
  | VBool (Left true) -> tcase
  | VBool (Left false) -> fcase
  | VBool (Right e) ->
      let@ (tenv,tstmts) = DisEnv.locally_ tcase in

      let@ env' = DisEnv.gets (fun env -> LocalEnv.sequence_merge env tenv) in
      let@ (fenv,fstmts) = DisEnv.locally_ (DisEnv.put env' >> fcase) in

      let@ () = DisEnv.join_locals tenv fenv in
      DisEnv.write [Stmt_If(Symbolic.lift_expr loc TC.type_bool e, tstmts, [], fstmts, loc)]
  | _ -> failwith ("Split on non-boolean value"))

let sym_and (loc: l) (x: sym rws) (y: sym rws): sym rws =
    sym_if loc TC.type_bool x y (DisEnv.pure Symbolic.vfalse)

let sym_or (loc: l) (x: sym rws) (y: sym rws): sym rws =
    sym_if loc TC.type_bool x (DisEnv.pure Symbolic.vtrue) y

(** Symbolic implementation of List.for_all2 *)
let rec sym_for_all2 p l1 l2 =
  match (l1, l2) with
  | ([], []) -> DisEnv.pure sym_true
  | (a1::l1, a2::l2) -> sym_if Unknown (type_bool) (p a1 a2) (sym_for_all2 p l1 l2) (DisEnv.pure sym_false)
  | (_, _) -> invalid_arg "sym_for_all2"

(** Symbolic implementation of List.exists *)
let rec sym_exists p = function
  | [] -> DisEnv.pure sym_false
  | [a] -> p a
  | a::l -> sym_or Unknown (p a) (sym_exists p l)

let width_of_type (loc: l) (t: ty): int =
  match t with
  | Type_Bits (Expr_LitInt wd) -> int_of_string wd
  | Type_Register (wd, _) -> int_of_string wd
  | _ -> unsupported loc @@ "Can't get bit width of type: " ^ pp_type t

let width_of_field (loc: l) (t: ty) (f: ident): int =
  let env = Tcheck.env0 in
  let ft =
    (match Tcheck.typeFields env loc t with
    | FT_Record rfs -> Tcheck.get_recordfield loc rfs f
    | FT_Register rfs -> let (_,t) = Tcheck.get_regfield loc rfs f in t)
  in
  width_of_type loc ft

(** Disassembly Functions *)

(** Determine the type of memory access expression (Var, Array, Field) *)
let rec type_of_load (loc: l) (x: expr): ty rws =
  let env = Tcheck.env0 in
  (match x with
  | Expr_Var(id) ->
      let+ (_,(t,_)) = DisEnv.gets (LocalEnv.resolveGetVar loc id) in
      t
  | Expr_Field(e,f) ->
      let@ t = type_of_load loc e in
      (match Tcheck.typeFields env loc t with
      | FT_Record rfs -> dis_type loc @@ Tcheck.get_recordfield loc rfs f
      | FT_Register rfs ->
        let (_,t) = Tcheck.get_regfield loc rfs f in
        dis_type loc t)
  | Expr_Array(a,i) ->
      let@ t = type_of_load loc a in
      (match Tcheck.derefType env t with
      | Type_Array(ixty, elty) -> dis_type loc elty
      | _ -> raise (EvalError (loc, "Can't type expression: " ^ pp_expr a)))
  | _ -> raise (EvalError (loc, "Can't type expression: " ^ pp_expr x)))

and type_access_chain (loc: l) (var: var) (ref: access_chain list): ty rws =
    let Var (_,id) = var in
    type_of_load loc (expr_access_chain (Expr_Var id) ref)

(** Disassemble type *)
and dis_type (loc: l) (t: ty): ty rws =
    match t with
    | Type_Bits ex ->
        let+ ex' = dis_expr loc ex in
        (Type_Bits (sym_expr ex'))
    | Type_OfExpr ex ->
        let+ ex' = dis_expr loc ex in
        (Type_OfExpr (sym_expr ex'))
    | Type_Tuple tys ->
        let+ exprs = DisEnv.traverse (dis_type loc) tys in
        (Type_Tuple exprs)
    | t' -> DisEnv.pure t'

(** Disassemble list of expressions *)
and dis_exprs (loc: l) (xs: AST.expr list): sym list rws =
    DisEnv.traverse (dis_expr loc) xs

(** Disassemble a pattern match, mirrors eval_pattern *)
and dis_pattern (loc: l) (v: sym) (x: AST.pattern): sym rws =
    (match x with
    | Pat_LitInt(l)  -> DisEnv.pure (sym_eq_int  loc v (Val (from_intLit l)))
    | Pat_LitHex(l)  -> DisEnv.pure (sym_eq_int  loc v (Val (from_hexLit l)))
    | Pat_LitBits(l) -> DisEnv.pure (sym_eq_bits loc v (Val (from_bitsLit l)))
    | Pat_LitMask(l) -> DisEnv.pure (sym_inmask  loc v (Val (from_maskLit l)))
    | Pat_Const(c)   ->
            let+ c' = dis_load loc (Expr_Var c) in
            sym_eq loc v c'
    | Pat_Wildcard   -> DisEnv.pure sym_true
    | Pat_Tuple(ps) ->
            let vs = sym_of_tuple loc v in
            assert (List.length vs = List.length ps);
            sym_for_all2 (dis_pattern loc) vs ps
    | Pat_Set(ps) ->
            sym_exists (dis_pattern loc v) ps
    | Pat_Single(e) ->
            let+ v' = dis_expr loc e in
            sym_eq loc v v'
    | Pat_Range(lo, hi) ->
            let+ lo' = dis_expr loc lo
            and+ hi' = dis_expr loc hi in
            sym_and_bool loc (sym_le_int loc lo' v) (sym_le_int loc v hi')
    )

(** Disassemble bitslice bounds, mirrors eval_slice *)
and dis_slice (loc: l) (x: slice): (sym * sym) rws =
    (match x with
    | Slice_Single(i) ->
            let+ i' = dis_expr loc i in
            (i', Val (VInt Z.one))
    | Slice_HiLo(hi, lo) ->
            let+ hi' = dis_expr loc hi
            and+ lo' = dis_expr loc lo in
            let wd' = sym_add_int loc (sym_sub_int loc hi' lo') (Val (VInt Z.one)) in
            (lo', wd')
    | Slice_LoWd(lo, wd) ->
            let+ lo' = dis_expr loc lo
            and+ wd' = dis_expr loc wd in
            (lo', wd')
    )

(**
  Disassemble a variable load, either accessing local or global state, potentially with a chain of
  aggregate structure accesses.

  Locals and globals are expected to all be defined by the symbolic state and assumed
  to never contain symbolic aggregate structures.
  These structures are not supported by the target and must be removed for successful translation.
  TODO: This does not appear to be a problem at the moment, but requires greater testing to be sure.
  *)
and dis_load loc x =
  DisEnv.scope loc "dis_load" (pp_expr x) pp_sym (dis_load_chain loc x [])

and dis_load_chain (loc: l) (x: expr) (ref: access_chain list): sym rws =
  (match x with
  | Expr_Var(id) ->
      let@ (var,local) = DisEnv.gets (LocalEnv.resolveGetVar loc id) in
      (match local with
      | (t, Val v) ->
          (* we assume that structures in the local state are always maximally
             expanded so this chain can always be evaluated, but may have
             uninitialised values at its base values. *)
          (match (get_access_chain loc v ref) with
          | VUninitialized _ ->
              let expr = expr_access_chain (var_expr var) ref in
              let@ t' = type_access_chain loc var ref in

              let@ var' = capture_expr_sym loc t' expr in
              (* if we are loading a bare symbolic variable,
                 update it to refer to the new captured expression
                 (as a minor optimisation). *)
              let@ () = DisEnv.if_ (ref = [])
                (DisEnv.modify (LocalEnv.setVar loc var var')) in
              DisEnv.pure var'
          | v' -> DisEnv.pure (Val v')
          )
      (* Variable is local with a symbolic value, should not expect a structure *)
      | (t, Exp e) ->
          if ref = [] then DisEnv.pure @@ Exp e
          else unsupported loc "Local variable with dynamic structure"
      )
  | Expr_Field(e,f) -> dis_load_chain loc e (Field f::ref)
  | Expr_Array(a,i) ->
      let@ i = dis_expr loc i in
      (match i with
      | Val i -> dis_load_chain loc a (Index i::ref)
      | _ -> unsupported loc "Dynamic array index")
  | x -> unsupported loc @@ "Unknown Exp chain: " ^ pp_expr x)

(** Dissassemble expression. This should never return Result VUninitialized *)
and dis_expr loc x =
  let+ r = DisEnv.scope loc "dis_expr" (pp_expr x) pp_sym (dis_expr' loc x) in
  match r with
  | Val (VUninitialized _) -> internal_error loc @@ "dis_expr returning VUninitialized, invalidating assumption"
  | _ -> r

and dis_expr' (loc: l) (x: AST.expr): sym rws =
    (match x with
    | Expr_If(ty, c, t, els, e) ->
            let rec eval_if xs d : sym rws = match xs with
                | [] -> dis_expr loc d
                | AST.E_Elsif_Cond (c,b)::xs' ->
                    sym_if loc ty (dis_expr loc c) (* then *)
                      (dis_expr loc b)
                    (* else *)
                      (eval_if xs' d)
            in
            eval_if (E_Elsif_Cond(c, t)::els) e
    | Expr_Binop(a, op, b) ->
            raise (EvalError (loc, "binary operation should have been removed in expression "
                   ^ Utils.to_string (PP.pp_expr x)))
    | Expr_Field(_, _) -> dis_load loc x
    | Expr_Fields(e, fs) ->
            let+ vs = DisEnv.traverse (fun f -> dis_load loc (Expr_Field(e,f))) fs in
            sym_concat loc vs
    | Expr_Slices(e, ss) ->
            let@ e' = dis_expr loc e
            and@ ss' = DisEnv.traverse (dis_slice loc) ss in
            let vs = List.map (fun (i,w) -> sym_extract_bits loc e' i w) ss' in
            DisEnv.pure (sym_concat loc vs)
    | Expr_In(e, p) ->
            let@ e' = dis_expr loc e in
            let@ p' = dis_pattern loc e' p in
            (match p' with
            | Val v -> DisEnv.pure (Val v)
            | Exp e -> capture_expr_sym loc type_bool e)
    | Expr_Var(_) -> dis_load loc x
    | Expr_Parens(e) ->
            dis_expr loc e
    | Expr_TApply(f, tes, es) ->
            if name_of_FIdent f = "and_bool" then begin
                (match (tes, es) with
                | ([], [x; y]) ->
                    sym_and loc (dis_expr loc x) (dis_expr loc y)
                | _ ->
                    internal_error loc @@ "malformed and_bool expression "
                       ^ Utils.to_string (PP.pp_expr x)
                )
            end else if name_of_FIdent f = "or_bool" then begin
                (match (tes, es) with
                | ([], [x; y]) ->
                    sym_or loc (dis_expr loc x) (dis_expr loc y)
                | _ ->
                    internal_error loc @@ "malformed or_bool expression "
                       ^ Utils.to_string (PP.pp_expr x)
                )
            end else if name_of_FIdent f = "implies_bool" then begin
                (match (tes, es) with
                | ([], [x; y]) ->
                    sym_if loc (type_bool) (dis_expr loc x) (* then *)
                      (dis_expr loc y)
                    (* else *)
                      (DisEnv.pure sym_true)
                | _ ->
                    internal_error loc @@ "malformed implies_bool expression "
                       ^ Utils.to_string (PP.pp_expr x)
                )
            end else begin
                let@ tvs = dis_exprs loc tes in
                let@ vs  = dis_exprs loc es in
                dis_funcall loc f tvs vs
            end
    | Expr_Tuple(es) ->
            let+ es' = DisEnv.traverse (dis_expr loc) es in
            sym_tuple es'
    | Expr_Unop(op, e) ->
            raise (EvalError (loc, "unary operation should have been removed"))
    | Expr_Unknown(t) -> (* TODO: Is this enough? *)
            let+ t' = dis_type loc t in
            Exp (Expr_Unknown(t'))
    | Expr_ImpDef(t, Some(s)) ->
            DisEnv.reads (fun env -> Val (Eval.Env.getImpdef loc env s))
    | Expr_ImpDef(t, None) ->
            raise (EvalError (loc, "unnamed IMPLEMENTATION_DEFINED behavior"))
    | Expr_Array(a,i) ->   dis_load loc x
    | Expr_LitInt(i) ->    DisEnv.pure (Val (from_intLit i))
    | Expr_LitHex(i) ->    DisEnv.pure (Val (from_hexLit i))
    | Expr_LitReal(r) ->   DisEnv.pure (Val (from_realLit r))
    | Expr_LitBits(b) ->   DisEnv.pure (Val (from_bitsLit b))
    | Expr_LitMask(b) ->   DisEnv.pure (Val (from_maskLit b))
    | Expr_LitString(s) -> DisEnv.pure (Val (from_stringLit s))
    )

(** Disassemble call to function *)
and dis_funcall (loc: l) (f: ident) (tvs: sym list) (vs: sym list): sym rws =
    let+ ret = dis_call loc f tvs vs in
    match ret with
    | None -> internal_error loc "function call finished without returning a value"
    | Some x -> x

(** Evaluate call to procedure *)
and dis_proccall (loc: l) (f: ident) (tvs: sym list) (vs: sym list): unit rws =
    let+ _ = dis_call loc f tvs vs in ()

(** Disassemble a function call *)
and dis_call (loc: l) (f: ident) (tes: sym list) (es: sym list): sym option rws =
    DisEnv.scope loc "dis_call"
        (pp_expr (Expr_TApply (f, List.map sym_expr tes, List.map sym_expr es)))
        (Option.fold ~none:"(no return)" ~some:pp_sym)
        (dis_call' loc f tes es)

and dis_call' (loc: l) (f: ident) (tes: sym list) (es: sym list): sym option rws =
    let@ fn = DisEnv.getFun loc f in
    (match fn with
    | Some (rty, targs, args, loc, b) ->
        let fname = name_of_FIdent f in

        (* Nest enviroment *)
        let@ () = DisEnv.modify LocalEnv.addLevel in

        assert (List.length targs == List.length tes);

        (* Assign targs := tes *)
        let@ () = DisEnv.sequence_ @@ List.map2 (fun arg e ->
            declare_const loc Additional.type_integer arg e
            ) targs tes in

        assert (List.length args == List.length es);

        (* Assign args := es *)
        let@ () = DisEnv.sequence_ (List.map2 (fun (ty, arg) e ->
            let@ ty' = dis_type loc ty in
            declare_const loc ty' arg e
        ) args es) in

        (* Create return variable (if necessary).
            This is in the inner scope to allow for type parameters. *)
        let@ rv = (match rty with
        | Some (Type_Tuple ts) ->
            let@ ts' = DisEnv.traverse (dis_type loc) ts in
            let+ names = DisEnv.traverse (declare_fresh_named_var loc fname) ts' in
            Some (Expr_Tuple (List.map var_expr_no_suffix_in_local_scope names))
        | Some t ->
            let@ t' = dis_type loc t in
            let+ name = declare_fresh_named_var loc fname t' in
            Some (var_expr_no_suffix_in_local_scope name)
        | None ->
            DisEnv.pure None) in

        let@ env = DisEnv.get in
        let@ () = DisEnv.if_ (!debug_level >= 2)
            (DisEnv.log (LocalEnv.pp_locals env ^ "\n")) in

        (* Evaluate body with new return symbol *)
        let@ () = DisEnv.modify (LocalEnv.addReturnSymbol rv) in
        let@ () = dis_stmts b in
        let@ () = DisEnv.modify (LocalEnv.removeReturnSymbol) in

        (* Disassemble return variable expression and propagate its symbolic value
            into the containing scope. *)
        let@ result = (match rv with
        | Some rv ->
            let+ result = dis_expr loc rv in
            Some result
        | None ->
            DisEnv.pure None) in
        (* Pop enviroment. *)
        let@ () = DisEnv.modify LocalEnv.popLevel in
        DisEnv.pure result
    | None ->
        let+ result = (dis_prim f tes es) in
        Some result
    )

and dis_prim (f: ident) (tes: sym list) (es: sym list): sym rws =
    let name = name_of_FIdent f in

    match sym_prim_simplify name tes es with
    | Some s -> DisEnv.pure s
    | None ->
        match sym_prim f tes es with
        | Exp f' ->
            if List.mem name Value.prims_pure
                then DisEnv.pure (Exp f')
                (* TODO(kl): avoid use of type_unknown by inferring return type of primitive. *)
                else let+ var = capture_expr Unknown type_unknown f' in var_sym_expr var
        | Val v -> DisEnv.pure (Val v)

and dis_lexpr loc x r: unit rws =
    DisEnv.scope loc
        "dis_lexpr" (pp_stmt (Stmt_Assign (x, sym_expr r, Unknown)))
        Additional.pp_unit
        (dis_lexpr' loc x r)

(** Remove potential effects from an lexpr *)
and resolve_lexpr (loc: l) (x: lexpr): lexpr rws =
  (match x with
  | LExpr_Field(l,f) ->
      let+ l = resolve_lexpr loc l in
      LExpr_Field(l,f)
  | LExpr_Array(l,i) ->
      let@ e = dis_expr loc i in
      let+ l = resolve_lexpr loc l in
      (match e with
      | Val i -> LExpr_Array(l, val_expr i)
      | _ -> unsupported loc @@ "Dynamic array index in LExpr")
  | _ -> DisEnv.pure(x))

(* TODO: Missing ReadWrite LExpr, which introduces some complications for Fields case *)
and dis_lexpr_chain (loc: l) (x: lexpr) (ref: access_chain list) (r: sym): unit rws =
  (match x with
  | LExpr_Field(l, f) -> dis_lexpr_chain loc l (Field f::ref) r
  | LExpr_Array(l, i) ->
      let@ e = dis_expr loc i in
      (match e with
      | Val i -> dis_lexpr_chain loc l (Index i::ref) r
      | _ -> unsupported loc @@ "Dynamic array index in LExpr")
  | LExpr_Var(id) ->
      let@ var,local = DisEnv.gets (LocalEnv.resolveGetVar loc id) in
      (match local with
      (* Base variable is local, can update as long as its primitive *)
      | (t,Val (VUninitialized _)) when ref <> [] ->
          (* if we reach here, the assumption that all structures are stored
             fully expanded has failed. that is, we incorrectly have a
             VUnitialized of a structure instead of a structure of uninitialised. *)
          internal_error loc @@
            "attempt to access field/index within invalid uninitialised structure."
      | (t,Val v) ->
          let@ () =
            (match ref with
            | _::_ ->
                (* if accessing inside structure, update local store with
                   new structure value. set to uninitialised if "r" is expression. *)
                let@ t' = type_access_chain loc var ref in
                let@ r' = sym_val_or_uninit t' r in
                let vv' = set_access_chain loc v ref r' in
                (* this loses propagation of pure expressions when they are assigned
                   into structures, but this is unavoidable since the structure value types
                   cannot store expressions. *)
                DisEnv.modify (LocalEnv.setVar loc var (Val vv'))
            | [] ->
                (* if accessing bare variable, just set its variable in local store. *)
                DisEnv.modify (LocalEnv.setVar loc var r)
          ) in
          (* possible failure if "r" is a record or array since those
             cannot be converted to expressions and assigned directly. *)
          DisEnv.write [Stmt_Assign(
            lexpr_access_chain (var_lexpr var) ref, sym_expr r, loc)]
      | (t,Exp e) ->
          (match ref with
          | _::_ ->
            (* variable contains a symbolic expression. read, modify, then write. *)
            let@ Var(_,tmp) = capture_expr_mutable loc t e in
            let@ () = dis_lexpr_chain loc (LExpr_Var tmp) ref r in
            let@ e' = dis_expr loc (Expr_Var tmp) in
            assign_var loc var e'
          | [] ->
            assign_var loc var r
          )
      )
  | _ -> unsupported loc @@ "Unknown LExpr modify constructor: " ^ pp_lexpr x)

and dis_lexpr' (loc: l) (x: lexpr) (r: sym): unit rws =
    (match x with
    | LExpr_Wildcard ->
        DisEnv.unit
    | LExpr_Var(v) ->
        dis_lexpr_chain loc x [] r
    | LExpr_Field(_,_) ->
        dis_lexpr_chain loc x [] r
    | LExpr_Fields(l,fs) ->
        let@ l = resolve_lexpr loc l in
        let@ ty = type_of_load loc (lexpr_to_expr loc l) in
        let rec set_fields (i: int) (fs: ident list): unit rws =
            (match fs with
            | [] -> DisEnv.unit
            | (f::fs') ->
                let w = width_of_field loc ty f in
                let y = sym_slice loc r i w in
                let@ () = dis_lexpr_chain loc l [Field f] y in
                set_fields (i + w) fs'
            )
        in
        set_fields 0 (List.rev fs)
    | LExpr_Slices(l, ss) ->
        let e = lexpr_to_expr loc l in
        let@ ty = type_of_load loc e in
        let prev_width = width_of_type loc ty in
        let rec eval (o: sym) (ss': AST.slice list) (prev: sym): sym rws =
            (match ss' with
            | [] -> DisEnv.pure prev
            | (s :: ss) ->
                let@ (i, w) = dis_slice loc s in
                let v       = sym_extract_bits loc r o w in
                eval (sym_add_int loc o w) ss (sym_insert_bits loc prev_width prev i w v)
            )
        in
        let@ old = dis_expr loc e in
        let@ rhs = eval (Val (VInt Z.zero)) ss old in
        dis_lexpr_chain loc l [] rhs
    | LExpr_Tuple(ls) ->
        let rs = sym_of_tuple loc r in
        assert (List.length ls = List.length rs);
        DisEnv.traverse2_ (dis_lexpr loc) ls rs
    | LExpr_Array(_,_) ->
        dis_lexpr_chain loc x [] r
    | LExpr_Write(setter, tes, es) ->
        let@ tvs = dis_exprs loc tes in
        let@ vs = dis_exprs loc es in
        dis_proccall loc setter tvs (vs @ [r])
    | _ -> unsupported loc @@ "Unknown LExpr constructor: " ^ pp_lexpr x)

(** Concatenates two lists of statements, but ensures nothing is
    added after a return statement.  *)
and stmt_append (xs: stmt list) (ys: stmt list): stmt list =
    match xs with
    | [] -> ys

    (* these interrupt control flow so we shouldn't append after them. *)
    | [Stmt_FunReturn _]
    | [Stmt_ProcReturn _]
    | [Stmt_Throw _]
    | [Stmt_Dep_Undefined _]
    | [Stmt_Undefined _] -> xs

    | x::xs -> x :: stmt_append xs ys

(** Dissassemble list of statements. *)
and dis_stmts (stmts: AST.stmt list): unit rws =
    match stmts with
    | [] -> DisEnv.unit
    | (Stmt_If(c, tstmts, elsif, fstmts, loc)::rest) ->
        (* append everything after the if statement into each of its branches. *)
        let tstmts' = stmt_append tstmts rest
        and elsif' = List.map (fun (S_Elsif_Cond(e,ss)) ->
            S_Elsif_Cond(e,stmt_append ss rest)) elsif
        and fstmts' = stmt_append fstmts rest in
        dis_stmt (Stmt_If (c, tstmts', elsif', fstmts', loc))
    | (Stmt_FunReturn _ | Stmt_ProcReturn _) as ret :: rest ->
        (match rest with
        | [] -> dis_stmt ret
        | _ -> raise (DisUnsupported (stmt_loc ret,
            "unexpected statements after return: " ^
            Additional.pp_list pp_stmt rest)))
    | (s::rest) ->
        dis_stmt s >> dis_stmts rest


(** Disassemble statement *)
and dis_stmt x = DisEnv.scope (stmt_loc x) "dis_stmt" (pp_stmt x) Additional.pp_unit (dis_stmt' x)
and dis_stmt' (x: AST.stmt): unit rws =
    (match x with
    | Stmt_VarDeclsNoInit(ty, vs, loc) ->
        (* If a local prefix exists, add it *)
        let@ ty' = dis_type loc ty in
        DisEnv.traverse_ (declare_var loc ty') vs
    | Stmt_VarDecl(ty, v, e, loc) ->
        (* If a local prefix exists, add it *)
        (* Add the variable *)
        let@ ty' = dis_type loc ty in
        let@ e' = dis_expr loc e in
        let@ _ = declare_assign_var loc ty' v e' in
        DisEnv.unit
    | Stmt_ConstDecl(ty, v, e, loc) ->
        (* If a local prefix exists, add it *)
        let@ ty' = dis_type loc ty in
        let@ e' = dis_expr loc e in
        let@ _ = declare_const loc ty' v e' in
        DisEnv.unit
    | Stmt_Assign(l, r, loc) ->
        let@ r' = dis_expr loc r in
        dis_lexpr loc l r'
    | Stmt_If(c, t, els, e, loc) ->
        let rec eval_if xs d : unit rws = match xs with
        | [] -> dis_stmts d
        | S_Elsif_Cond (c,b)::xs' ->
            unit_if loc (dis_expr loc c)
              (dis_stmts b)
            (* else *)
              (eval_if xs' d)
        in
        eval_if (S_Elsif_Cond(c, t)::els) e
    | Stmt_TCall (f, tes, es, loc) ->
        let@ tes' = dis_exprs loc tes in
        let@ es' = dis_exprs loc es in
        dis_proccall loc f tes' es'
    | Stmt_FunReturn(e, loc) ->
        let@ rv = DisEnv.gets (LocalEnv.getReturnSymbol loc) in
        let@ e' = dis_expr loc e in
        dis_lexpr loc (expr_to_lexpr rv) e'
    | Stmt_ProcReturn(loc) -> DisEnv.unit
    | Stmt_Assert(e, loc) ->
        let@ e' = dis_expr loc e in
        (match e' with
        | Val v ->
            if not (to_bool loc v) then
                raise (EvalError (loc, "assertion failure during symbolic phase"))
            else
                DisEnv.unit
        | Exp e'' ->
            DisEnv.write [Stmt_Assert(e'', loc)]
        )
    | Stmt_Case(e, alts, odefault, loc) ->
        let rec dis_alts (alts: alt list) (d: stmt list option) (v: sym): unit rws = (
            let@ () = DisEnv.unit in (* force function to be lazily evaluated when rws is computed. *)
            match alts with
            | [] -> (match d with
                (* cannot throw here because this may be reached by disassembling a
                   case with unknown expressions.
                   should only throw an exception at runtime if does not match. *)
                | None -> DisEnv.write [Stmt_See (Expr_LitString "unmatched case", loc)]
                | Some s -> dis_stmts s)
            | Alt_Alt(ps, oc, s) :: alts' ->
                let pat = (sym_exists (dis_pattern loc v) ps) in
                let pat_and_guard =
                    (match oc with
                    | Some c -> sym_and loc pat (dis_expr loc c)
                    | None -> pat)
                in
                (unit_if loc pat_and_guard
                    (dis_stmts s)
                    (dis_alts alts' d v))
        ) in
        let@ e' = dis_expr loc e in
        dis_alts alts odefault e'
    | Stmt_For(var, start, dir, stop, body, loc) ->
        let@ start' = dis_expr loc start in
        let@ stop' = dis_expr loc stop in

        (match (start', stop') with
        | Val startval, Val stopval ->
            let rec dis_for (i: value): unit rws =
                let c = (match dir with
                | Direction_Up -> eval_leq loc i stopval
                | Direction_Down -> eval_leq loc stopval i
                ) in
                if c then
                    let i' = (match dir with
                    | Direction_Up   -> eval_add_int loc i (VInt Z.one)
                    | Direction_Down -> eval_sub_int loc i (VInt Z.one)
                    ) in
                    let@ () = dis_stmts ([Stmt_Assign (LExpr_Var var, val_expr i, loc)] @ body)
                    in
                    dis_for i'
                else
                    DisEnv.unit
            in
            declare_var loc Additional.type_integer var >>
            dis_for startval
        | _, _ ->
            raise (DisUnsupported (loc, "for loop bounds not statically known: " ^ pp_stmt x)))
    | Stmt_Unpred _
    | Stmt_ConstrainedUnpred _
    | Stmt_ImpDef (_, _)
    | Stmt_Undefined _
    | Stmt_ExceptionTaken _
    | Stmt_Dep_Unpred _
    | Stmt_Dep_ImpDef (_, _)
    | Stmt_Dep_Undefined _
    | Stmt_See (_, _)
    | Stmt_Throw (_, _)
    | Stmt_DecodeExecute (_, _, _)
    | Stmt_While (_, _, _)
    | Stmt_Repeat (_, _, _)
    | Stmt_Try (_, _, _, _, _) ->
        (* need to defer raising so this does not throw in unreachable cases. *)
        let@ () = DisEnv.unit in
        raise (DisUnsupported (stmt_loc x, "dis_stmt: unsupported statement: " ^ pp_stmt x))
    )

let dis_encoding (x: encoding) (op: value): bool rws =
    let Encoding_Block (nm, iset, fields, opcode, guard, unpreds, b, loc) = x in
    (* todo: consider checking iset *)
    (* Printf.printf "Checking opcode match %s == %s\n" (Utils.to_string (PP.pp_opcode_value opcode)) (pp_value op); *)
    let ok = (match opcode with
    | Opcode_Bits b -> eval_eq     loc op (from_bitsLit b)
    | Opcode_Mask m -> eval_inmask loc op (from_maskLit m)
    ) in
    if ok then begin
        if !Eval.trace_instruction then Printf.printf "TRACE: instruction %s\n" (pprint_ident nm);

        let@ () = DisEnv.traverse_ (function (IField_Field (f, lo, wd)) ->
            let v = extract_bits' loc op lo wd in
            if !Eval.trace_instruction then Printf.printf "      %s = %s\n" (pprint_ident f) (pp_value v);
            declare_assign_var Unknown (val_type v) f (Val v)
        ) fields in

        let@ guard' = dis_expr loc guard in
        if to_bool loc (sym_value_unsafe guard') then begin
            List.iter (fun (i, b) ->
                if eval_eq loc (extract_bits' loc op i 1) (from_bitsLit b) then
                    raise (Throw (loc, Exc_Unpredictable))
            ) unpreds;
            (* dis_encoding: we cannot guarantee that these statements are fully evaluated. *)
            let@ () = dis_stmts b in
            DisEnv.pure true
        end else begin
            DisEnv.pure false
        end
    end else begin
        DisEnv.pure false
    end

let dis_decode_slice (loc: l) (x: decode_slice) (op: value): value rws =
    (match x with
    | DecoderSlice_Slice (lo, wd) ->
        DisEnv.pure @@ extract_bits' loc op lo wd
    | DecoderSlice_FieldName f ->
        (* assumes expression always evaluates to concrete value. *)
        let+ _,f' = DisEnv.getVar loc f in sym_value_unsafe f'
    | DecoderSlice_Concat fs ->
        (* assumes expression always evaluates to concrete value. *)
        let+ fs' = DisEnv.traverse (DisEnv.getVar loc) fs in
        eval_concat loc (List.map (fun (_,s) -> sym_value_unsafe s) fs')
    )

(* Duplicate of eval_decode_case modified to print rather than eval *)
let rec dis_decode_case (loc: AST.l) (x: decode_case) (op: value): unit rws =
    DisEnv.scope loc "dis_decode_case" (Additional.pp_decode_case x) Additional.pp_unit
        (dis_decode_case' loc x op)
and dis_decode_case' (loc: AST.l) (x: decode_case) (op: value): unit rws =
    (match x with
    | DecoderCase_Case (ss, alts, loc) ->
            let@ vs = DisEnv.traverse (fun s -> dis_decode_slice loc s op) ss in
            let rec dis alts =
                (match alts with
                | (alt :: alts') ->
                    let@ alt' = dis_decode_alt loc alt vs op in
                    (match alt' with
                    | true -> DisEnv.unit
                    | false -> dis alts')
                | [] ->
                        raise (DisInternalError (loc, "unmatched decode pattern"))
                )
            in
            dis alts
    )

(* Duplicate of eval_decode_alt modified to print rather than eval *)
and dis_decode_alt (loc: l) (x: decode_alt) (vs: value list) (op: value): bool rws =
    DisEnv.scope loc "dis_decode_alt" (Additional.pp_decode_alt x) string_of_bool
        (dis_decode_alt' loc x vs op)
and dis_decode_alt' (loc: AST.l) (DecoderAlt_Alt (ps, b)) (vs: value list) (op: value): bool rws =
    if List.for_all2 (Eval.eval_decode_pattern loc) ps vs then
        (match b with
        | DecoderBody_UNPRED loc -> raise (Throw (loc, Exc_Unpredictable))
        | DecoderBody_UNALLOC loc -> raise (Throw (loc, Exc_Undefined))
        | DecoderBody_NOP loc -> DisEnv.pure true
        | DecoderBody_Encoding (inst, l) ->
                let@ (enc, opost, cond, exec) = DisEnv.reads (fun env -> Eval.Env.getInstruction loc env inst) in
                let@ enc_match = dis_encoding enc op in
                if enc_match then begin
                    (* todo: should evaluate ConditionHolds to decide whether to execute body *)
                    let@ env = DisEnv.read in

                    if !debug_level >= 1 then begin
                        Printf.printf "Dissasm: %s\n" (pprint_ident inst);
                    end;

                    let opost' = (match opost with
                    | Some post ->
                        Printf.printf "also disassembling __postdecode...\n";
                        post
                    | None -> []
                    ) in
                    let@ (lenv',stmts) = DisEnv.locally_ (
                        let@ () = DisEnv.modify (LocalEnv.addLevel) in
                        let@ () = dis_stmts (opost' @ exec) in
                        DisEnv.modify (LocalEnv.popLevel)
                    ) in

                    if !debug_level >= 2 then begin
                        Printf.printf "-----------\n";
                        List.iter (fun s -> Printf.printf "%s\n" (pp_stmt s)) stmts;
                        Printf.printf "-----------\n";
                    end;

                    let@ () = DisEnv.write stmts in
                    DisEnv.pure true
                end else begin
                    DisEnv.pure false
                end
        | DecoderBody_Decoder (fs, c, loc) ->
                let@ () = DisEnv.modify (LocalEnv.addLevel) in
                let@ () = DisEnv.traverse_ (function (IField_Field (f, lo, wd)) ->
                    let v = extract_bits' loc op lo wd in
                    declare_assign_var loc (val_type v) f (Val v)
                ) fs
                in
                let@ () = dis_decode_case loc c op in
                let@ () = DisEnv.modify (LocalEnv.popLevel) in
                DisEnv.pure true
        )
    else
        DisEnv.pure false

let dis_decode_entry (env: Eval.Env.t) (decode: decode_case) (op: value): stmt list =
    let DecoderCase_Case (_,_,loc) = decode in

    let env = Eval.Env.freeze env in
    let globals = IdentSet.of_list @@ List.map fst @@ Bindings.bindings (Eval.Env.readGlobals env) in
    let lenv = LocalEnv.init env in
    let ((),lenv',stmts) = (dis_decode_case loc decode op) env lenv in
    let stmts' = Transforms.RemoveUnused.remove_unused globals @@ stmts in
    (* let stmts' = Transforms.Bits.bitvec_conversion stmts' in *)
    if !debug_level >= 2 then begin
        Printf.printf "===========\n";
        List.iter (fun s -> Printf.printf "%s\n" (pp_stmt s)) stmts';
        Printf.printf "===========\n";
    end;
    let stmts' = Transforms.IntToBits.ints_to_bits stmts' in
    stmts'


let retrieveDisassembly (env: Eval.Env.t) (opcode: string): stmt list =
    let decoder = Eval.Env.getDecoder env (Ident "A64") in
    let DecoderCase_Case (_,_,loc) = decoder in
    (* List.iter (fun (ident, _) -> Eval.Env.setVar Unknown env ident VUninitialized) (Bindings.bindings (Eval.Env.getGlobals env).bs); *)
    dis_decode_entry env decoder (Value.VBits (Primops.prim_cvt_int_bits (Z.of_int 32) (Z.of_int (int_of_string opcode))))

let init (env: Eval.Env.t): LocalEnv.t =
    let eval e = val_expr (Eval.eval_expr Unknown env e) in
    let tenv = Tcheck.env0 in
    let get_global_type id =
        (match Tcheck.GlobalEnv.getGlobalVar tenv id with
        | Some (Type_Bits e) ->
            (Type_Bits (eval e))
        | Some (Type_App (i, es)) ->
            (Type_App (i, List.map eval es))
        | Some t -> (t)
        | _ -> internal_error Unknown @@ "cannot find type for global: " ^ pprint_ident id)
    in

    let globals = Eval.Env.readGlobals env in
    let consts = Eval.Env.readGlobalConsts env in

    let merge_left k l r = Some l in
    let globalsAndConsts = Bindings.union merge_left globals consts
    in
    let globals = Bindings.mapi
        (fun id v -> (get_global_type id, Val v))
        globalsAndConsts
    in
    {
        locals = [Bindings.empty ; globals];
        returnSymbols = [];
        numSymbols = 0;
        indent = 0;
        trace = [];
    }

let sym_pure_prim (f: string) (tvs: sym list) (vs: sym list): value option =
    let open Primops in
    let f' : type a. a Symbolic.sym = Either.Right (ECall (FIdent (f,0), tvs, vs)) in
    let b' : Z.t Symbolic.sym -> vbits = fun n -> Right {n=n; v=ECall (FIdent (f,0), tvs, vs)} in
    ( match (f, tvs, vs) with
    | ("eq_enum",           [      ], [VInt (Left x); VInt (Left y)    ])     -> Some (VBool   (Left(x = y)))
    | ("eq_enum",           [      ], [VInt _       ; VInt _           ])     -> Some (VBool   (f'))
    | ("eq_enum",           [      ], [VBool (Left x); VBool (Left y)    ])     -> Some (VBool   (Left(x = y)))
    | ("eq_enum",           [      ], [VBool _       ; VBool _           ])     -> Some (VBool   (f'))
    | ("ne_enum",           [      ], [VInt (Left x); VInt (Left y)    ])     -> Some (VBool   (Left(x <> y)))
    | ("ne_enum",           [      ], [VInt _       ; VInt _           ])     -> Some (VBool   (f'))
    | ("ne_enum",           [      ], [VBool (Left x); VBool (Left y)    ])     -> Some (VBool   (Left(x <> y)))
    | ("ne_enum",           [      ], [VBool _       ; VBool _           ])     -> Some (VBool   (f'))
    | ("eq_bool",           [      ], [VBool (Left x); VBool (Left y)    ])     -> Some (VBool   (Left(prim_eq_bool    x y)))
    | ("eq_bool",           [      ], [VBool _       ; VBool _           ])     -> Some (VBool   (f'))
    | ("ne_bool",           [      ], [VBool (Left x); VBool (Left y)    ])     -> Some (VBool   (Left(prim_ne_bool    x y)))
    | ("ne_bool",           [      ], [VBool _       ; VBool _           ])     -> Some (VBool   (f'))
    | ("equiv_bool",        [      ], [VBool (Left x); VBool (Left y)    ])     -> Some (VBool   (Left(prim_equiv_bool x y)))
    | ("equiv_bool",        [      ], [VBool _       ; VBool _           ])     -> Some (VBool   (f'))
    | ("not_bool",          [      ], [VBool (Left x)             ])     -> Some (VBool   (Left(prim_not_bool x)))
    | ("not_bool",          [      ], [VBool _                    ])     -> Some (VBool   (f'))
    | ("eq_int",            [      ], [VInt  (Left x); VInt  (Left y)    ])     -> Some (VBool   (Left(prim_eq_int     x y)))
    | ("eq_int",            [      ], [VInt  _       ; VInt  _           ])     -> Some (VBool   (f'))
    | ("ne_int",            [      ], [VInt  (Left x); VInt  (Left y)    ])     -> Some (VBool   (Left(prim_ne_int     x y)))
    | ("ne_int",            [      ], [VInt  _       ; VInt  _           ])     -> Some (VBool   (f'))
    | ("le_int",            [      ], [VInt  (Left x); VInt  (Left y)    ])     -> Some (VBool   (Left(prim_le_int     x y)))
    | ("le_int",            [      ], [VInt  _       ; VInt  _           ])     -> Some (VBool   (f'))
    | ("lt_int",            [      ], [VInt  (Left x); VInt  (Left y)    ])     -> Some (VBool   (Left(prim_lt_int     x y)))
    | ("lt_int",            [      ], [VInt  _       ; VInt  _           ])     -> Some (VBool   (f'))
    | ("ge_int",            [      ], [VInt  (Left x); VInt  (Left y)    ])     -> Some (VBool   (Left(prim_ge_int     x y)))
    | ("ge_int",            [      ], [VInt  _       ; VInt  _           ])     -> Some (VBool   (f'))
    | ("gt_int",            [      ], [VInt  (Left x); VInt  (Left y)    ])     -> Some (VBool   (Left(prim_gt_int     x y)))
    | ("gt_int",            [      ], [VInt  _       ; VInt  _           ])     -> Some (VBool   (f'))
    | ("is_pow2_int",       [      ], [VInt  (Left x)             ])     -> Some (VBool   (Left(prim_is_pow2_int x)))
    | ("is_pow2_int",       [      ], [VInt  _                    ])     -> Some (VBool   (f'))
    | ("neg_int",           [      ], [VInt  (Left x)             ])     -> Some (VInt    (Left(prim_neg_int    x)))
    | ("neg_int",           [      ], [VInt  _                    ])     -> Some (VInt    (f'))
    | ("add_int",           [      ], [VInt  (Left x); VInt  (Left y)    ])     -> Some (VInt    (Left(prim_add_int    x y)))
    | ("add_int",           [      ], [VInt  _       ; VInt  _           ])     -> Some (VInt    (f'))
    | ("sub_int",           [      ], [VInt  (Left x); VInt  (Left y)    ])     -> Some (VInt    (Left(prim_sub_int    x y)))
    | ("sub_int",           [      ], [VInt  _       ; VInt  _           ])     -> Some (VInt    (f'))
    | ("shl_int",           [      ], [VInt  (Left x); VInt  (Left y)    ])     -> Some (VInt    (Left(prim_shl_int    x y)))
    | ("shl_int",           [      ], [VInt  _       ; VInt  _           ])     -> Some (VInt    (f'))
    | ("shr_int",           [      ], [VInt  (Left x); VInt  (Left y)    ])     -> Some (VInt    (Left(prim_shr_int    x y)))
    | ("shr_int",           [      ], [VInt  _       ; VInt  _           ])     -> Some (VInt    (f'))
    | ("mul_int",           [      ], [VInt  (Left x); VInt  (Left y)    ])     -> Some (VInt    (Left(prim_mul_int    x y)))
    | ("mul_int",           [      ], [VInt  _       ; VInt  _           ])     -> Some (VInt    (f'))
    | ("zdiv_int",          [      ], [VInt  (Left x); VInt  (Left y)    ])     -> Some (VInt    (Left(prim_zdiv_int   x y)))
    | ("zdiv_int",          [      ], [VInt  _       ; VInt  _           ])     -> Some (VInt    (f'))
    | ("zrem_int",          [      ], [VInt  (Left x); VInt  (Left y)    ])     -> Some (VInt    (Left(prim_zrem_int   x y)))
    | ("zrem_int",          [      ], [VInt  _       ; VInt  _           ])     -> Some (VInt    (f'))
    | ("fdiv_int",          [      ], [VInt  (Left x); VInt  (Left y)    ])     -> Some (VInt    (Left(prim_fdiv_int   x y)))
    | ("fdiv_int",          [      ], [VInt  _       ; VInt  _           ])     -> Some (VInt    (f'))
    | ("frem_int",          [      ], [VInt  (Left x); VInt  (Left y)    ])     -> Some (VInt    (Left(prim_frem_int   x y)))
    | ("frem_int",          [      ], [VInt  _       ; VInt  _           ])     -> Some (VInt    (f'))
    | ("mod_pow2_int",      [      ], [VInt  (Left x); VInt  (Left y)    ])     -> Some (VInt    (Left(prim_mod_pow2_int x y)))
    | ("mod_pow2_int",      [      ], [VInt  _       ; VInt  _           ])     -> Some (VInt    (f'))
    | ("align_int",         [      ], [VInt  (Left x); VInt  (Left y)    ])     -> Some (VInt    (Left(prim_align_int    x y)))
    | ("align_int",         [      ], [VInt  _       ; VInt  _           ])     -> Some (VInt    (f'))
    | ("pow2_int",          [      ], [VInt  (Left x)             ])     -> Some (VInt    (Left(prim_pow2_int     x)))
    | ("pow2_int",          [      ], [VInt  _                    ])     -> Some (VInt    (f'))
    | ("pow_int_int",       [      ], [VInt  (Left x); VInt  (Left y)    ])     -> Some (VInt    (Left(prim_pow_int_int  x y)))
    | ("pow_int_int",       [      ], [VInt  _       ; VInt  _           ])     -> Some (VInt    (f'))
    | ("cvt_int_real",      [      ], [VInt (Left x)              ])     -> Some (VReal   (Left(prim_cvt_int_real x)))
    | ("cvt_int_real",      [      ], [VInt _                     ])     -> Some (VReal   (f'))
    | ("eq_real",           [      ], [VReal (Left x); VReal (Left y)    ])     -> Some (VBool   (Left(prim_eq_real x y)))
    | ("eq_real",           [      ], [VReal _       ; VReal _           ])     -> Some (VBool   (f'))
    | ("ne_real",           [      ], [VReal (Left x); VReal (Left y)    ])     -> Some (VBool   (Left(prim_ne_real x y)))
    | ("ne_real",           [      ], [VReal _       ; VReal _           ])     -> Some (VBool   (f'))
    | ("le_real",           [      ], [VReal (Left x); VReal (Left y)    ])     -> Some (VBool   (Left(prim_le_real x y)))
    | ("le_real",           [      ], [VReal _       ; VReal _           ])     -> Some (VBool   (f'))
    | ("lt_real",           [      ], [VReal (Left x); VReal (Left y)    ])     -> Some (VBool   (Left(prim_lt_real x y)))
    | ("lt_real",           [      ], [VReal _       ; VReal _           ])     -> Some (VBool   (f'))
    | ("ge_real",           [      ], [VReal (Left x); VReal (Left y)    ])     -> Some (VBool   (Left(prim_ge_real x y)))
    | ("ge_real",           [      ], [VReal _       ; VReal _           ])     -> Some (VBool   (f'))
    | ("gt_real",           [      ], [VReal (Left x); VReal (Left y)    ])     -> Some (VBool   (Left(prim_gt_real x y)))
    | ("gt_real",           [      ], [VReal _       ; VReal _           ])     -> Some (VBool   (f'))
    | ("add_real",          [      ], [VReal (Left x); VReal (Left y)    ])     -> Some (VReal   (Left(prim_add_real x y)))
    | ("add_real",          [      ], [VReal _       ; VReal _           ])     -> Some (VReal   (f'))
    | ("neg_real",          [      ], [VReal (Left x)             ])     -> Some (VReal   (Left(prim_neg_real x)))
    | ("neg_real",          [      ], [VReal _                    ])     -> Some (VReal   (f'))
    | ("sub_real",          [      ], [VReal (Left x); VReal (Left y)    ])     -> Some (VReal   (Left(prim_sub_real x y)))
    | ("sub_real",          [      ], [VReal _       ; VReal _           ])     -> Some (VReal   (f'))
    | ("mul_real",          [      ], [VReal (Left x); VReal (Left y)    ])     -> Some (VReal   (Left(prim_mul_real x y)))
    | ("mul_real",          [      ], [VReal _       ; VReal _           ])     -> Some (VReal   (f'))
    | ("divide_real",       [      ], [VReal (Left x); VReal (Left y)    ])     -> Some (VReal   (Left(prim_div_real x y)))
    | ("divide_real",       [      ], [VReal _       ; VReal _           ])     -> Some (VReal   (f'))
    | ("pow2_real",         [      ], [VInt  (Left x)             ])     -> Some (VReal   (Left(prim_pow2_real x)))
    | ("pow2_real",         [      ], [VInt  _                    ])     -> Some (VReal   (f'))
    | ("round_tozero_real", [      ], [VReal (Left x)             ])     -> Some (VInt    (Left(prim_round_tozero_real x)))
    | ("round_tozero_real", [      ], [VReal _                    ])     -> Some (VInt    (f'))
    | ("round_down_real",   [      ], [VReal (Left x)             ])     -> Some (VInt    (Left(prim_round_down_real x)))
    | ("round_down_real",   [      ], [VReal _                    ])     -> Some (VInt    (f'))
    | ("round_up_real",     [      ], [VReal (Left x)             ])     -> Some (VInt    (Left(prim_round_up_real x)))
    | ("round_up_real",     [      ], [VReal _                    ])     -> Some (VInt    (f'))
    | ("sqrt_real",         [      ], [VReal (Left x); VReal (Left y)    ])     -> Some (VReal   (Left(prim_sqrt_real x)))
    | ("sqrt_real",         [      ], [VReal _       ; VReal _           ])     -> Some (VReal   (f'))
    | ("cvt_int_bits",      [_     ], [VInt  (Left x); VInt  (Left n)    ])     -> Some (VBits   (Left(prim_cvt_int_bits n x)))
    | ("cvt_int_bits",      [VInt n], [VInt  _       ; VInt  _           ])     -> Some (VBits   (b' n))
    | ("cvt_bits_sint",     [VInt n], [VBits (Left x)             ])     -> Some (VInt    (Left(prim_cvt_bits_sint x)))
    | ("cvt_bits_sint",     [VInt n], [VBits _                    ])     -> Some (VInt    (f'))
    | ("cvt_bits_uint",     [VInt n], [VBits (Left x)             ])     -> Some (VInt    (Left(prim_cvt_bits_uint x)))
    | ("cvt_bits_uint",     [VInt n], [VBits _                    ])     -> Some (VInt    (f'))
    | ("in_mask",           [VInt n], [VBits (Left x); VMask (Left y)    ])     -> Some (VBool  (Left(prim_in_mask x y)))
    | ("in_mask",           [VInt n], [VBits _       ; VMask _           ])     -> Some (VBool  (f'))
    | ("notin_mask",        [VInt n], [VBits (Left x); VMask (Left y)    ])     -> Some (VBool  (Left(prim_notin_mask x y)))
    | ("notin_mask",        [VInt n], [VBits _       ; VMask _           ])     -> Some (VBool  (f'))
    | ("eq_bits",           [VInt n], [VBits (Left x); VBits (Left y)    ])     -> Some (VBool  (Left(prim_eq_bits x y)))
    | ("eq_bits",           [VInt n], [VBits _       ; VBits _           ])     -> Some (VBool  (f'))
    | ("ne_bits",           [VInt n], [VBits (Left x); VBits (Left y)    ])     -> Some (VBool  (Left(prim_ne_bits x y)))
    | ("ne_bits",           [VInt n], [VBits _       ; VBits _           ])     -> Some (VBool  (f'))
    | ("add_bits",          [VInt n], [VBits (Left x); VBits (Left y)    ])     -> Some (VBits  (Left(prim_add_bits x y)))
    | ("add_bits",          [VInt n], [VBits _       ; VBits _           ])     -> Some (VBits  (b' n))
    | ("sub_bits",          [VInt n], [VBits (Left x); VBits (Left y)    ])     -> Some (VBits  (Left(prim_sub_bits x y)))
    | ("sub_bits",          [VInt n], [VBits _       ; VBits _           ])     -> Some (VBits  (b' n))
    | ("mul_bits",          [VInt n], [VBits (Left x); VBits (Left y)    ])     -> Some (VBits  (Left(prim_mul_bits x y)))
    | ("mul_bits",          [VInt n], [VBits _       ; VBits _           ])     -> Some (VBits  (b' n))
    | ("and_bits",          [VInt n], [VBits (Left x); VBits (Left y)    ])     -> Some (VBits  (Left(prim_and_bits x y)))
    | ("and_bits",          [VInt n], [VBits _       ; VBits _           ])     -> Some (VBits  (b' n))
    | ("or_bits",           [VInt n], [VBits (Left x); VBits (Left y)    ])     -> Some (VBits  (Left(prim_or_bits x y)))
    | ("or_bits",           [VInt n], [VBits _       ; VBits _           ])     -> Some (VBits  (b' n))
    | ("eor_bits",          [VInt n], [VBits (Left x); VBits (Left y)    ])     -> Some (VBits  (Left(prim_eor_bits x y)))
    | ("eor_bits",          [VInt n], [VBits _       ; VBits _           ])     -> Some (VBits  (b' n))
    | ("not_bits",          [VInt n], [VBits (Left x)             ])     -> Some (VBits   (Left(prim_not_bits x)))
    | ("not_bits",          [VInt n], [VBits _                    ])     -> Some (VBits   (b' n))
    | ("zeros_bits",        [VInt (Left n)], [                    ])     -> Some (VBits   (Left(prim_zeros_bits n)))
    | ("ones_bits",         [VInt (Left n)], [                    ])     -> Some (VBits   (Left(prim_ones_bits n)))
    | ("replicate_bits",    [_; _  ], [VBits (Left x); VInt (Left y)     ])     -> Some (VBits   (Left(prim_replicate_bits x y)))
    | ("replicate_bits",    [VInt m; VInt n], [VBits _       ; VInt _            ])     -> Some (VBits   (b' (sym_mul_int m n)))
    | ("append_bits",       [VInt m; VInt n], [VBits (Left x); VBits (Left y)]) -> Some (VBits   (Left(prim_append_bits x y)))
    | ("append_bits",       [VInt m; VInt n], [VBits _       ; VBits _       ]) -> Some (VBits   (b' (sym_add_int m n)))
    | ("eq_str",            [      ], [VString (Left x); VString (Left y)])     -> Some (VBool   (Left(prim_eq_str x y)))
    | ("eq_str",            [      ], [VString _       ; VString _       ])     -> Some (VBool   (f'))
    | ("ne_str",            [      ], [VString (Left x); VString (Left y)])     -> Some (VBool   (Left(prim_ne_str x y)))
    | ("ne_str",            [      ], [VString _       ; VString _       ])     -> Some (VBool   (f'))
    | ("append_str_str",    [      ], [VString (Left x); VString (Left y)])     -> Some (VString (Left(prim_append_str x y)))
    | ("append_str_str",    [      ], [VString _       ; VString _       ])     -> Some (VString (f'))
    | ("cvt_int_hexstr",    [      ], [VInt (Left x)              ])     -> Some (VString (Left(prim_cvt_int_hexstr x)))
    | ("cvt_int_hexstr",    [      ], [VInt _                     ])     -> Some (VString (f'))
    | ("cvt_int_decstr",    [      ], [VInt (Left x)              ])     -> Some (VString (Left(prim_cvt_int_decstr x)))
    | ("cvt_int_decstr",    [      ], [VInt _                     ])     -> Some (VString (f'))
    | ("cvt_bool_str",      [      ], [VBool (Left x)             ])     -> Some (VString (Left(prim_cvt_bool_str x)))
    | ("cvt_bool_str",      [      ], [VBool _                    ])     -> Some (VString (f'))
    | ("cvt_bits_str",      [_     ], [VInt (Left n);    VBits (Left x)  ])     -> Some (VString (Left(prim_cvt_bits_str n x)))
    | ("cvt_bits_str",      [_     ], [VInt _       ;    VBits _         ])     -> Some (VString (f'))
    | ("cvt_real_str",      [      ], [VReal (Left x)             ])     -> Some (VString (Left(prim_cvt_real_str x)))
    | ("cvt_real_str",      [      ], [VReal _                    ])     -> Some (VString (f'))
    | ("is_cunpred_exc",    [      ], [VExc (_, ex)        ])     -> Some (VBool  (Left (prim_is_cunpred_exc ex)))
    | ("is_exctaken_exc",   [      ], [VExc (_, ex)        ])     -> Some (VBool  (Left (prim_is_exctaken_exc ex)))
    | ("is_impdef_exc",     [      ], [VExc (_, ex)        ])     -> Some (VBool  (Left (prim_is_impdef_exc ex)))
    | ("is_see_exc",        [      ], [VExc (_, ex)        ])     -> Some (VBool  (Left (prim_is_see_exc ex)))
    | ("is_undefined_exc",  [      ], [VExc (_, ex)        ])     -> Some (VBool  (Left (prim_is_undefined_exc ex)))
    | ("is_unpred_exc",     [      ], [VExc (_, ex)        ])     -> Some (VBool  (Left (prim_is_unpred_exc ex)))
    | _ -> None
    )

let sym_prim f tes es =
    match sym_pure_prim f tes es with
    | Some x -> DisEnv.pure (Some x)
    | None -> unsupported Unknown "unsupported non-pure primitive"

type fun_sig = Abstract_interface.fun_sig
type inst_sig = Abstract_interface.inst_sig

module Dis : Abstract_interface.Effect with type value = Symbolic.value = struct
  type 'a eff = 'a rws
  type value = sym

  let dis_TYPE_UNKNOWN = Type_Constructor (Ident "unknown")

  (* Monadic *)
  let pure  : 'a -> 'a eff = DisEnv.pure
  let (>>=) : 'a eff -> ('a -> 'b eff) -> 'b eff = DisEnv.bind
  let (>>) : 'a eff -> ('a -> 'b) -> 'b eff = fun x f -> DisEnv.fmap f x
  let traverse : ('a -> 'b eff) -> 'a list -> 'b list eff = DisEnv.traverse

  let (let*) = (>>=)

  (** sequence effects, returning second result  *)
  let (>>>) x y = let* _ = x in y

  (** sequence effects, returning first result  *)
  let (<<<) x y = let* x' = x in (y >>> pure x')

  (* State *)
  let reset : unit eff = (DisEnv.read >> init) >>= DisEnv.put
  let scope : 'a eff -> 'a eff =
    fun x -> DisEnv.modify LocalEnv.addLevel >>> x <<< DisEnv.modify LocalEnv.popLevel
  let call  : unit eff -> value eff =
    fun body ->
      let@ rv = declare_fresh_named_var Unknown "Return" (Type_Constructor (Ident "unknown")) in

      DisEnv.modify (LocalEnv.addLevel) >>>
      let@ () = DisEnv.modify (LocalEnv.addReturnSymbol rv) in
      let@ () = body in
      let@ () = DisEnv.modify (LocalEnv.removeReturnSymbol) in

      (* Disassemble return variable expression and propagate its symbolic value
          into the containing scope. *)
      let@ (_,result) = DisEnv.gets (LocalEnv.getVar Unknown rv) in
      (* Pop enviroment. *)
      let@ () = DisEnv.modify LocalEnv.popLevel in
      DisEnv.pure result

  let setVar : AST.l -> AST.ident -> value -> unit eff =
    fun l nm x -> DisEnv.modify (LocalEnv.resolveSetVar l nm x)
  let runPrim : string -> value list -> value list -> value option eff = sym_prim
  let isGlobalConstFilter : (AST.ident -> bool) eff =
    let+ lenv = DisEnv.get in
    fun nm ->
      LocalEnv.resolveVar Unknown nm lenv |> fun (Var(i,_)) -> i = 0

  let getGlobalConst      : AST.ident -> value eff =
    fun nm -> DisEnv.gets (LocalEnv.resolveGetVar Unknown nm) >> snd >> snd
  let getVar              : AST.l -> AST.ident -> value eff =
    fun l nm -> DisEnv.gets (LocalEnv.resolveGetVar l nm) >> snd >> snd
  let getImpdef           : AST.l -> string -> value eff =
    fun l nm ->
      let+ env = DisEnv.read in
      Symbolic.from_value (Eval.Env.getImpdef l env nm)
  let getFun              : AST.l -> AST.ident -> fun_sig eff =
    fun l nm ->
      let+ env = DisEnv.read in (Eval.Env.getFun l env nm)
  let getInstruction      : AST.l -> AST.ident -> inst_sig eff =
    fun l nm ->
      let+ env = DisEnv.read in (Eval.Env.getInstruction l env nm)
  let getDecoder          : AST.ident -> AST.decode_case eff =
    fun nm ->
      let+ env = DisEnv.read in (Eval.Env.getDecoder env nm)
  let getEnum             : AST.ident -> value list option eff =
    fun nm ->
      let+ env = DisEnv.read in
      let enum = Eval.Env.getEnum env nm in
      Option.map (List.map Symbolic.from_value) enum

  let getRecord           : AST.ident -> (AST.ty * AST.ident) list option eff =
    fun nm ->
      let+ env = DisEnv.read in
      Eval.Env.getRecord env nm
  let getTypedef          : AST.ident -> AST.ty option eff =
    fun nm ->
      let+ env = DisEnv.read in
      Eval.Env.getTypedef env nm

  let addRecord      : AST.ident -> (AST.ty * AST.ident) list -> unit eff =
    fun nm fields ->
      let+ env = DisEnv.read in
      Eval.Env.addRecord env nm fields
  let addGlobalConst : AST.ident -> value -> unit eff =
    fun nm x -> DisEnv.stateful (LocalEnv.addLocalConst Unknown nm x dis_TYPE_UNKNOWN) >>> pure ()
  let addGlobalVar   : AST.ident -> value -> unit eff =
    fun nm x -> DisEnv.stateful (LocalEnv.addLocalVar Unknown nm x dis_TYPE_UNKNOWN) >>> pure ()
  let addEnum        : AST.ident -> value list -> unit eff =
    fun nm fields ->
      let+ env = DisEnv.read in
      Eval.Env.addEnum env nm (List.map Symbolic.to_value fields)
  let addTypedef     : AST.ident -> AST.ty -> unit eff =
    fun nm ty ->
      let+ env = DisEnv.read in
      Eval.Env.addTypedef env nm ty
  let addDecoder     : AST.ident -> AST.decode_case -> unit eff =
    fun nm dec ->
      let+ env = DisEnv.read in
      Eval.Env.addDecoder env nm dec
  let addInstruction : AST.l -> AST.ident -> inst_sig -> unit eff =
    fun l nm inst ->
      let+ env = DisEnv.read in
      Eval.Env.addInstruction l env nm inst
  let addFun         : AST.l -> AST.ident -> fun_sig -> unit eff =
    fun l nm func ->
      let+ env = DisEnv.read in
      Eval.Env.addFun l env nm func

  let addLocalVar    : AST.l -> AST.ident -> value -> unit eff =
    fun l nm x -> DisEnv.stateful (LocalEnv.addLocalVar l nm x dis_TYPE_UNKNOWN) >>> pure ()
  let addLocalConst  : AST.l -> AST.ident -> value -> unit eff =
    fun l nm x -> DisEnv.stateful (LocalEnv.addLocalVar l nm x dis_TYPE_UNKNOWN) >>> pure ()

  (* Control Flow *)
  let branch    : value -> 'a eff -> 'a eff -> 'a eff = assert false
  let rec iter      : ('a -> ('a * value) eff) -> 'a -> 'a eff =
    fun go x ->
      let* (x', continue) = go x in
      branch continue (iter go x') (pure x')
  let return    : value -> 'a eff =
    fun v ->
      let@ rv = DisEnv.gets (LocalEnv.getReturnSymbol Unknown) in
      let@ (_,e') = DisEnv.gets (LocalEnv.getVar Unknown rv) in
      DisEnv.modify (LocalEnv.setVar Unknown rv e') >>>
      (* dis_lexpr loc (expr_to_lexpr rv) e' *)
      assert false
  let throw     : AST.l -> Primops.exc -> 'a eff = assert false
  let catch     : 'a eff -> (AST.l -> Primops.exc -> 'a eff) -> 'a eff = assert false
  let error     : AST.l -> string -> 'a eff = assert false

end
