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
(* let var_sym_expr v = Exp (var_expr v) *)

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

    let newLocalVar (loc: l) (k: ident) (env: t): var =
        Var (List.length env.locals - 1, k)

    (** Adds a new local variable to the innermost scope. *)
    let addLocalVar (loc: l) (k: ident) (v: sym) (t: ty) (env: t): var * t =
        if !Eval.trace_write then Printf.printf "TRACE: fresh %s = %s\n" (pprint_ident k) (pp_sym v);
        let var = newLocalVar loc k env in
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
        (* let tcenv = TC.env0 in *)
        pure (VTuple [])
        (* TODO: implement generation of empty record types from this thing *)

        (* let+ env = read in
        try
            Eval.mk_uninitialized Unknown env t
        with
            e -> unsupported Unknown @@
                "mkUninit: failed to evaluate type " ^ pp_type t ^ " due to " ^
                Printexc.to_string e *)

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
                        | false ->
                            (* TODO: recurse and compare leafs of Value datatype *)
                            (* replacing scalar values with EUnknown if conflicting. *)
                            pure v1

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
(* let to_expr = sym_expr *)

(** Converts a result_or_simplified to a value.
    Raises an exception if an expression is given, as an expression cannot be casted to a value.
    Requires checking beforehand *)
let to_value (v: sym): value =
    assert false
    (* match v with
    | Val v' -> v'
    | Exp _ -> raise (EvalError (Unknown, "Unreachable")) *)

let is_expr (v: sym): bool =
    assert false
    (* match v with
    | Val _ -> false
    | Exp _ -> true *)

let declare_var (loc: l) (t: ty) (i: ident): var rws =
  let@ env = DisEnv.read in
  (* let@ uninit = DisEnv.mkUninit t in *)
  let@ var = DisEnv.gets (LocalEnv.newLocalVar loc i) in
  (* let@ var = DisEnv.stateful
    (LocalEnv.addLocalVar loc i (Val uninit) t) in *)
  let+ () = DisEnv.write [Stmt_VarDeclsNoInit(t, [var_ident var], loc)] in
  var

let declare_assign_var (loc: l) (t: ty) (i: ident) (x: sym): var rws =
  let@ var = DisEnv.stateful
    (LocalEnv.addLocalVar loc i x t) in
  let+ () = DisEnv.write [Stmt_VarDecl(t, var_ident var, to_expr loc x, loc)] in
  var

let declare_fresh_named_var (loc: l) (name: string) (t: ty): var rws =
  let@ res = DisEnv.nextVarName name in
  declare_var loc t res

let assign_var (loc: l) (i: var) (x: sym): unit rws =
  DisEnv.modify (LocalEnv.setVar loc i x) >>
  DisEnv.write [Stmt_Assign(LExpr_Var(var_ident i), to_expr loc x, loc)]

let declare_const (loc: l) (i: ident) (x: sym): var rws =
  (* let@ var = DisEnv.stateful
    (LocalEnv.addLocalConst loc i x t) in *)
  (* let+ () = DisEnv.write [Stmt_ConstDecl(t, var_ident var, to_expr loc x, loc)] in *)
  DisEnv.gets (LocalEnv.newLocalVar loc i)

let declare_fresh_const (loc: l) (name: string) (x: expr): var rws =
  let@ i = DisEnv.nextVarName name in
  declare_const loc i (assert false)

(* Captures the given expression and returns a resolved variable to the captured
   name. *)
let capture_expr loc x: var rws =
  let@ v = declare_fresh_const loc "Exp" x in
  (* let@ uninit = DisEnv.mkUninit t in *)
  (* let+ () = DisEnv.modify (LocalEnv.setVar loc v (Val uninit)) in *)
  DisEnv.pure v

let rec sym_visit_exprs  (f: Symbolic.expr -> Symbolic.expr rws) (x: Symbolic.value): sym rws =
  match x with
  | VBool (Left _) -> DisEnv.pure x
  | VInt (Left _) -> DisEnv.pure x
  | VReal (Left _) -> DisEnv.pure x
  | VBits (Left _) -> DisEnv.pure x
  | VMask (Left _) -> DisEnv.pure x
  | VString (Left _) -> DisEnv.pure x
  | VRAM (Left _) -> DisEnv.pure x
  | VExc _ -> DisEnv.pure x
  | VTuple (vs) ->
    let+ vs = DisEnv.traverse (sym_visit_exprs f) vs in
    VTuple vs
  | VRecord (vs) ->
    let+ vs' = DisEnv.traverse
        (fun (k,v) -> let+ v' = sym_visit_exprs f v in (k,v'))
        (Bindings.bindings vs) in
    VRecord (Bindings.of_seq (List.to_seq vs'))
  | VArray (vs,d) ->
    let@ vs' = DisEnv.traverse
        (fun (k,v) -> let+ v' = sym_visit_exprs f v in (k,v'))
        (Primops.ImmutableArray.bindings vs) in
    let+ d' = sym_visit_exprs f d in
    VArray (Primops.ImmutableArray.of_seq (List.to_seq vs'), d')
  | VBool (Right x) -> let+ x' = f x in VBool (Right x')
  | VInt (Right x) -> let+ x' = f x in VInt (Right x')
  | VReal (Right x) -> let+ x' = f x in VReal (Right x')
  | VBits (Right {n=Left n; v=x}) -> let+ x' = f x in VBits (Right {n=Left n; v=x'})
  | VBits (Right {n=Right n; v=x}) ->
        let@ n' = sym_visit_exprs f (VInt (Right n)) in
        let+ x' = f x in
        VBits (Right {n=to_int Unknown n'; v=x'})
  | VMask (Right x) -> let+ x' = f x in VMask (Right x')
  | VString (Right x) -> let+ x' = f x in VString (Right x')
  | VRAM (Right x) -> let+ x' = f x in VRAM (Right x')

(* recurse into var and replace all EUnknown with an EVar which has been captured *)
let capture_var (var: var): sym rws =
  let Var (level,_) = var in
  let unknown : bool ref = ref false in

  let f : Symbolic.expr -> Symbolic.expr rws =
    function
    | EUnknown ->
        unknown := true;
        let+ name = DisEnv.nextVarName "Var" in
        var_expr (Var (level, name))
    | x -> DisEnv.pure x
  in

  let@ _,x = DisEnv.gets (LocalEnv.getVar Unknown var) in
  let@ x = sym_visit_exprs f x in
  DisEnv.pure x
  (* TODO: work out how to capture EUnknown values into a variable from deep within an object *)



(** Monadic Utilities *)



(** Symbolic implementation of an if statement that returns an expression
 *)
let sym_if (loc: l) (test: sym) (tcase: 'a rws) (fcase: 'b rws): sym rws =
  (match test with
  | (VBool (Left true))  -> tcase
  | (VBool (Left false)) -> fcase
  | (VBool (Right e)) ->
      let@ tmp = declare_fresh_named_var loc "If" (TC.type_exn) in
      (* Evaluate true branch statements. *)
      let@ (_,tenv,tstmts) = DisEnv.locally
          (let@ x = tcase in let+ () = assign_var loc tmp x in x) in
      (* Propagate incremented counter to env'. *)
      let@ env' = DisEnv.gets (fun env -> LocalEnv.sequence_merge env tenv) in
      (* Execute false branch statements with env'. *)
      let@ (fenv,fstmts) = DisEnv.locally_
          (DisEnv.put env' >> fcase >>= assign_var loc tmp) in
      let@ () = DisEnv.join_locals tenv fenv in
      let@ () = DisEnv.write [Stmt_If(Symbolic.lift_expr loc TC.type_bool e, tstmts, [], fstmts, loc)] in
      (* Construct a value matching the value constructor of the true branch's result. *)
      (* Symbolic.copy_type tval (var_expr tmp) *)
      capture_var tmp
  | _ -> failwith ("Split on non-boolean value")
  )

(* let retrieveDisassembly (env: Eval.Env.t) (opcode: string): stmt list =
    let decoder = Eval.Env.getDecoder env (Ident "A64") in
    let DecoderCase_Case (_,_,loc) = decoder in
    (* List.iter (fun (ident, _) -> Eval.Env.setVar Unknown env ident VUninitialized) (Bindings.bindings (Eval.Env.getGlobals env).bs); *)
    dis_decode_entry env decoder (Value.VBits (Primops.prim_cvt_int_bits (Z.of_int 32) (Z.of_int (int_of_string opcode)))) *)

let init (env: Eval.Env.t): LocalEnv.t =
    (* let eval e = (assert false) (Eval.eval_expr Unknown env e) in
    let tenv = Tcheck.env0 in
    let get_global_type id =
        (match Tcheck.GlobalEnv.getGlobalVar tenv id with
        | Some (Type_Bits e) ->
            (Type_Bits (eval e))
        | Some (Type_App (i, es)) ->
            (Type_App (i, List.map eval es))
        | Some t -> (t)
        | _ -> internal_error Unknown @@ "cannot find type for global: " ^ pprint_ident id)
    in *)

    let globals = Eval.Env.readGlobals env in
    let consts = Eval.Env.readGlobalConsts env in

    let merge_left k l r = Some l in
    let globalsAndConsts = Bindings.union merge_left globals consts
    in
    let globals = Bindings.mapi
        (fun id v -> (TC.type_exn, from_value v))
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
    (* | None -> unsupported Unknown "unsupported non-pure primitive" *)
    | None -> DisEnv.pure None
    (* TODO: non-pure primitive functions *)

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

  let branch    : value -> value eff Lazy.t -> value eff Lazy.t -> value eff =
    fun c t f -> sym_if Unknown c (Lazy.force t) (Lazy.force f)

  let rec iter      : ('a -> ('a * value) eff) -> 'a -> 'a eff =
    fun go x ->
      let* (x', continue) = go x in
      branch continue (lazy (iter go x')) (Lazy.from_val @@ pure x')
  let return    : value -> 'a eff =
    fun v ->
      let@ rv = DisEnv.gets (LocalEnv.getReturnSymbol Unknown) in
      let@ (_,e') = DisEnv.gets (LocalEnv.getVar Unknown rv) in
      assign_var Unknown rv e' >>>
      (* dis_lexpr loc (expr_to_lexpr rv) e' *)
      assert false
  let throw     : AST.l -> Primops.exc -> 'a eff =
    fun l e ->
        DisEnv.write [Stmt_Throw (Ident (Additional.pp_exc e), l)]
        >>>
        assert false
  let catch     : 'a eff -> (AST.l -> Primops.exc -> 'a eff) -> 'a eff = assert false
  let error     : AST.l -> string -> 'a eff =
    fun _ s -> failwith @@ "error: " ^ s

end
