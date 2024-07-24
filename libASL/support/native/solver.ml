(****************************************************************)
(** {3 Z3 support code}                                         *)
(****************************************************************)

(** Convert ASL expression to Z3 expression.
    This only copes with a limited set of operations: ==, +, -, * and DIV.
    (It is possible that we will need to extend this list in the future but
    it is sufficient for the current ASL specifications.)

    The support for DIV is not sound - it is a hack needed to cope with
    the way ASL code is written and generally needs a side condition
    that the division is exact (no remainder).

    ufs is a mutable list of conversions used to handle subexpressions
    that cannot be translated.  We treat such subexpressions as
    uninterpreted functions and add them to the 'ufs' list so that
    we can reason that "F(x) == F(x)" without knowing "F".
 *)

module AST = LibASL_stage0.Asl_ast
module Asl_utils = LibASL_stage0.Asl_utils

let verbose = false

let rec z3_of_expr (ctx: Z3.context) (ufs: (AST.expr * Z3.Expr.expr) list ref) (x: AST.expr): Z3.Expr.expr =
    (match x with
    | Expr_Var(v) ->
        let intsort = Z3.Arithmetic.Integer.mk_sort ctx in
        Z3.Expr.mk_const_s ctx (AST.pprint_ident v) intsort
    | Expr_Parens y -> z3_of_expr ctx ufs y
    | Expr_LitInt i -> Z3.Arithmetic.Integer.mk_numeral_s ctx i

    (* todo: the following lines involving DIV are not sound *)
    | Expr_TApply (FIdent ("mul_int",_), [], [Expr_TApply (FIdent ("fdiv_int",_), [], [a; b]); c]) when b = c -> z3_of_expr ctx ufs a
    | Expr_TApply (FIdent ("mul_int",_), [], [a; Expr_TApply (FIdent ("fdiv_int",_), [], [b; c])]) when a = c -> z3_of_expr ctx ufs b
    | Expr_TApply (FIdent ("add_int",_), [], [Expr_TApply (FIdent ("fdiv_int",_), [], [a1; b1]);
                                         Expr_TApply (FIdent ("fdiv_int",_), [], [a2; b2])])
         when a1 = a2 && b1 = b2 && b1 = Expr_LitInt "2"
         -> z3_of_expr ctx ufs a1
    | Expr_TApply (FIdent ("eq_int",_), [], [a; Expr_TApply (FIdent ("fdiv_int",_), [], [b; c])]) ->
            Z3.Boolean.mk_eq ctx
                (Z3.Arithmetic.mk_mul ctx [z3_of_expr ctx ufs c; z3_of_expr ctx ufs a])
                (z3_of_expr ctx ufs b)

    | Expr_TApply (FIdent ("add_int",_),  [], xs)    -> Z3.Arithmetic.mk_add ctx (List.map (z3_of_expr ctx ufs) xs)
    | Expr_TApply (FIdent ("sub_int",_),  [], xs)    -> Z3.Arithmetic.mk_sub ctx (List.map (z3_of_expr ctx ufs) xs)
    | Expr_TApply (FIdent ("mul_int",_),  [], xs)    -> Z3.Arithmetic.mk_mul ctx (List.map (z3_of_expr ctx ufs) xs)
    | Expr_TApply (FIdent ("fdiv_int",_), [], [a;b]) -> Z3.Arithmetic.mk_div ctx (z3_of_expr ctx ufs a) (z3_of_expr ctx ufs b)
    | Expr_TApply (FIdent ("eq_int",_),   [], [a;b]) -> Z3.Boolean.mk_eq ctx (z3_of_expr ctx ufs a) (z3_of_expr ctx ufs b)
    | _ ->
            if verbose then Printf.printf "    Unable to translate %s - using as uninterpreted function\n" (Asl_utils.pp_expr x);
            let intsort = Z3.Arithmetic.Integer.mk_sort ctx in
            (match List.assoc_opt x !ufs with
            | None ->
                    let uf = Z3.Expr.mk_fresh_const ctx "UNINTERPRETED" intsort in
                    ufs := (x, uf) :: !ufs;
                    uf
            | Some uf ->
                    uf
            )
    )

(** check that bs => cs *)
let check_constraints (bs: AST.expr list) (cs: AST.expr list): bool =
    (* note that we rebuild the Z3 context each time.
     * It is possible to share them across all invocations to save
     * about 10% of execution time.
     *)
    let z3_ctx = Z3.mk_context [] in
    let solver = Z3.Solver.mk_simple_solver z3_ctx in
    let ufs = ref [] in (* uninterpreted function list *)
    let bs' = List.map (z3_of_expr z3_ctx ufs) bs in
    let cs' = List.map (z3_of_expr z3_ctx ufs) cs in
    let p = Z3.Boolean.mk_implies z3_ctx (Z3.Boolean.mk_and z3_ctx bs') (Z3.Boolean.mk_and z3_ctx cs') in
    if verbose then Printf.printf "      - Checking %s\n" (Z3.Expr.to_string p);
    Z3.Solver.add solver [Z3.Boolean.mk_not z3_ctx p];
    let q = Z3.Solver.check solver [] in
    if q = SATISFIABLE then Printf.printf "Failed property %s\n" (Z3.Expr.to_string p);
    q = UNSATISFIABLE
