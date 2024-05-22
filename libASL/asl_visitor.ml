(****************************************************************
 * ASL visitor class
 *
 * Copyright Arm Limited (c) 2017-2019
 * SPDX-Licence-Identifier: BSD-3-Clause
 *
 * This code follows the pattern used in the cilVisitor class in
 * George Necula's excellent CIL (https://people.eecs.berkeley.edu/~necula/cil/)
 * and makes use of the generic Visitor module that is copied from CIL.
 ****************************************************************)

(** ASL visitor class *)

open Asl_ast
open Visitor

(****************************************************************)
(** {2 ASL visitor class}                                       *)
(****************************************************************)

(** For each datatype in the ASL AST, a visitor defines what actions
    it wants to perform on values of that type.
 *)

class type aslVisitor = object

    method vvar      : ident          -> ident          visitAction
    method ve_elsif  : e_elsif        -> e_elsif        visitAction
    method vslice    : slice          -> slice          visitAction
    method vpattern  : pattern        -> pattern        visitAction
    method vexpr     : expr           -> expr           visitAction
    method vtype     : ty             -> ty             visitAction
    method vlvar     : ident          -> ident          visitAction
    method vlexpr    : lexpr          -> lexpr          visitAction
    method vstmt     : stmt           -> stmt list      visitAction
    method vs_elsif  : s_elsif        -> s_elsif        visitAction
    method valt      : alt            -> alt            visitAction
    method vcatcher  : catcher        -> catcher        visitAction
    method vmapfield : mapfield       -> mapfield       visitAction
    method vsformal  : sformal        -> sformal        visitAction
    method vdpattern : decode_pattern -> decode_pattern visitAction
    method vencoding : encoding       -> encoding       visitAction
    method vdcase    : decode_case    -> decode_case    visitAction
    method vdalt     : decode_alt     -> decode_alt     visitAction
    method vdbody    : decode_body    -> decode_body    visitAction
    method vdecl     : declaration    -> declaration    visitAction

    method enter_scope : (ty * ident) list -> unit
    method leave_scope : unit -> unit
end

(** Converts a visitAction on single values to an action on lists.
    The generated visitAction will throw if given a non-singleton list. *)
let singletonVisitAction (a: 'a visitAction) : 'a list visitAction =
    let listpost post : 'a list -> 'a list = function
        | [x] -> [post x]
        | xs ->
            let len = string_of_int @@ List.length xs in
            failwith @@ "this ChangeDoChildrenPost handles single values only, but was given a list of " ^ len ^ " items"
    in match a with
    | ChangeTo x -> ChangeTo [x]
    | ChangeDoChildrenPost(x, post) -> ChangeDoChildrenPost([x], listpost post)
    | DoChildren -> DoChildren
    | SkipChildren -> SkipChildren


(****************************************************************)
(** {2 ASL visitor functions}                                   *)
(****************************************************************)

(** The following set of recursive functions are the ASL specific
    part of the visitor class.
    For each data constructor of each datatype, they invoke visitors
    on each field of the data constructor and then reconstruct
    the corresponding data constructor.

    These functions implement the space-saving optimisation of
    only reconstructing the constructor if the sub-values are
    different.
 *)

let arg_of_sformal (sf: sformal): (ty * ident) =
    match sf with
    | Formal_In (ty, id)
    | Formal_InOut (ty, id) -> (ty, id)

let arg_of_ifield (IField_Field (id, _, wd)): (ty * ident) =
    (Type_Bits (Expr_LitInt (string_of_int wd)), id)

let args_of_encoding (Encoding_Block (_, _, fs, _, _, _, _, _)): (ty * ident) list =
    List.map arg_of_ifield fs

(** a base class for treeVisitors transforming the AST.
    the method visit_stmts is left abstract for subclasses
    to implement. *)
class virtual aslTreeVisitor (vis: #aslVisitor) = object(self)

    method visit_exprs (xs: expr list): expr list =
        mapNoCopy (self#visit_expr) xs

    method visit_var (x: ident): ident =
        let aux (_: #aslVisitor) (x: ident): ident =
            x
        in
        doVisit vis (vis#vvar x) aux x

    method visit_lvar (x: ident): ident =
        let aux (_: #aslVisitor) (x: ident): ident =
            x
        in
        doVisit vis (vis#vlvar x) aux x

    method visit_e_elsif (x: e_elsif): e_elsif =
        let aux (_: #aslVisitor) (x: e_elsif): e_elsif =
            (match x with
            | E_Elsif_Cond(c, e) ->
                    let c' = self#visit_expr c in
                    let e' = self#visit_expr e in
                    if c == c' && e == e' then x else E_Elsif_Cond(c', e')
            )
        in
        doVisit vis (vis#ve_elsif x) aux x

    method visit_slice (x: slice): slice =
        let aux (_: #aslVisitor) (x: slice): slice =
            (match x with
            | Slice_Single(e) ->
                    let e' = self#visit_expr e in
                    if e == e' then x else Slice_Single e'
            | Slice_HiLo(hi, lo) ->
                    let hi' = self#visit_expr hi in
                    let lo' = self#visit_expr lo in
                    if hi == hi' && lo == lo' then x else Slice_HiLo(hi', lo')
            | Slice_LoWd(lo, wd) ->
                    let lo' = self#visit_expr lo in
                    let wd' = self#visit_expr wd in
                    if lo == lo' && wd == wd' then x else Slice_LoWd(lo', wd')
            )
        in
        doVisit vis (vis#vslice x) aux x

    method visit_patterns (xs: pattern list): pattern list =
        mapNoCopy (self#visit_pattern) xs

    method visit_pattern (x: pattern): pattern =
        let aux (_: #aslVisitor) (x: pattern): pattern =
            ( match x with
            | Pat_LitInt(_)  -> x
            | Pat_LitHex(_)  -> x
            | Pat_LitBits(_) -> x
            | Pat_LitMask(_) -> x
            | Pat_Const(_)   -> x
            | Pat_Wildcard   -> x
            | Pat_Tuple(ps)  ->
                    let ps' = self#visit_patterns ps in
                    if ps == ps' then x else Pat_Tuple ps'
            | Pat_Set(ps) ->
                    let ps' = self#visit_patterns ps in
                    if ps == ps' then x else Pat_Set ps'
            | Pat_Single(e) ->
                let e' = self#visit_expr e in
                if e == e' then x else Pat_Single(e')
            | Pat_Range(lo, hi) ->
                let lo' = self#visit_expr lo in
                let hi' = self#visit_expr hi in
                if lo == lo' && hi == hi' then x else Pat_Range(lo', hi')
            )
        in
        doVisit vis (vis#vpattern x) aux x

    method visit_expr (x: expr): expr =
        let aux (_: #aslVisitor) (x: expr): expr =
            (match x with
            | Expr_If(ty, c, t, els, e) ->
                    let ty   = self#visit_type ty in
                    let c'   = self#visit_expr c in
                    let t'   = self#visit_expr t in
                    let els' = mapNoCopy (self#visit_e_elsif) els in
                    let e'   = self#visit_expr e in
                    if c == c' && t == t' && els == els' && e == e' then x else Expr_If(ty, c', t', els', e')
            | Expr_Binop(a, op, b) ->
                    let a' = self#visit_expr a in
                    let b' = self#visit_expr b in
                    if a == a' && b == b' then x else Expr_Binop(a', op, b')
            | Expr_Field(e, f) ->
                    let e' = self#visit_expr e in
                    if e == e' then x else Expr_Field(e', f)
            | Expr_Fields(e, fs) ->
                    let e' = self#visit_expr e in
                    if e == e' then x else Expr_Fields(e', fs)
            | Expr_Slices(e, ss) ->
                    let e'  = self#visit_expr e in
                    let ss' = mapNoCopy (self#visit_slice) ss in
                    if e == e' && ss == ss' then x else Expr_Slices(e', ss')
            | Expr_In(e, p) ->
                    let e' = self#visit_expr e in
                    let p' = self#visit_pattern p in
                    if e == e' && p == p' then x else Expr_In(e', p')
            | Expr_Var(v) ->
                    let v' = self#visit_var v in
                    if v == v' then x else Expr_Var(v')
            | Expr_Parens(e) ->
                    let e' = self#visit_expr e in
                    if e == e' then x else Expr_Parens e'
            | Expr_TApply(f, tes, es) ->
                    let tes' = self#visit_exprs tes in
                    let es'  = self#visit_exprs es in
                    if tes == tes' && es == es' then x else Expr_TApply(f, tes', es')
            | Expr_Tuple(es) ->
                    let es'  = self#visit_exprs es in
                    if es == es' then x else Expr_Tuple es'
            | Expr_Unop(op, e) ->
                    let e' = self#visit_expr e in
                    if e == e' then x else Expr_Unop(op, e')
            | Expr_Unknown(t) ->
                    let t' = self#visit_type t in
                    if t == t' then x else Expr_Unknown t'
            | Expr_ImpDef(t, os) ->
                    let t' = self#visit_type t in
                    if t == t' then x else Expr_ImpDef(t', os)
            | Expr_Array(a, e) ->
                    let a' = self#visit_expr a in
                    let e' = self#visit_expr e in
                    if a == a' && e == e' then x else Expr_Array(a', e')
            | Expr_LitInt    _  -> x
            | Expr_LitHex    _  -> x
            | Expr_LitReal   _  -> x
            | Expr_LitBits   _  -> x
            | Expr_LitMask   _  -> x
            | Expr_LitString _  -> x
            )
        in
        doVisit vis (vis#vexpr x) aux x


    method visit_types (xs: ty list): ty list =
        mapNoCopy (self#visit_type) xs

    method visit_type (x: ty): ty =
        let aux (_: #aslVisitor) (x: ty): ty =
            ( match x with
            | Type_Constructor(_) -> x
            | Type_Bits(n) ->
                    let n' = self#visit_expr n in
                    if n == n' then x else Type_Bits(n')
            | Type_App(tc, es) ->
                    let es' = self#visit_exprs es in
                    if es == es' then x else Type_App(tc, es')
            | Type_OfExpr(e) ->
                    let e' = self#visit_expr e in
                    if e == e' then x else Type_OfExpr(e')
            | Type_Register(wd, fs) ->
                    let fs' = mapNoCopy (fun ((ss, f) as r) ->
                        let ss' = mapNoCopy (self#visit_slice) ss in
                        if ss == ss' then r else (ss', f)
                    ) fs in
                    if fs == fs' then x else Type_Register(wd, fs')
            | Type_Array(Index_Enum(tc), ety) ->
                    let ety' = self#visit_type ety in
                    if ety == ety' then x else Type_Array(Index_Enum(tc), ety')
            | Type_Array(Index_Range(lo, hi), ety) ->
                    let lo' = self#visit_expr lo in
                    let hi' = self#visit_expr hi in
                    let ety' = self#visit_type ety in
                    if lo == lo' && hi == hi' && ety == ety' then x else Type_Array(Index_Range(lo',hi'),ety')
            | Type_Tuple(tys) ->
                    let tys' = self#visit_types tys in
                    if tys == tys' then x else Type_Tuple(tys')
            )
        in
        doVisit vis (vis#vtype x) aux x

    method visit_lexprs (xs: lexpr list): lexpr list =
        mapNoCopy (self#visit_lexpr) xs

    method visit_lexpr (x: lexpr): lexpr =
        let aux (_: #aslVisitor) (x: lexpr): lexpr =
            ( match x with
            | LExpr_Wildcard   -> x
            | LExpr_Var(v) ->
                    let v' = self#visit_lvar v in
                    if v == v' then x else LExpr_Var(v')
            | LExpr_Field(e, f) ->
                    let e' = self#visit_lexpr e in
                    if e == e' then x else LExpr_Field(e', f)
            | LExpr_Fields(e, fs) ->
                    let e' = self#visit_lexpr e in
                    if e == e' then x else LExpr_Fields(e', fs)
            | LExpr_Slices(e, ss) ->
                    let e'  = self#visit_lexpr e in
                    let ss' = mapNoCopy (self#visit_slice) ss in
                    if e == e' && ss == ss' then x else LExpr_Slices(e', ss')
            | LExpr_BitTuple(es)  ->
                    let es' = mapNoCopy (self#visit_lexpr) es in
                    if es == es' then x else LExpr_BitTuple es'
            | LExpr_Tuple(es)  ->
                    let es' = mapNoCopy (self#visit_lexpr) es in
                    if es == es' then x else LExpr_Tuple es'
            | LExpr_Array(a, e) ->
                    let a' = self#visit_lexpr a in
                    let e' = self#visit_expr e in
                    if a == a' && e == e' then x else LExpr_Array(a', e')
            | LExpr_Write(f, tes, es) ->
                    let f'   = self#visit_var f in
                    let tes' = self#visit_exprs tes in
                    let es'  = self#visit_exprs es in
                    if f == f' && tes == tes' && es == es' then x else LExpr_Write(f, tes', es')
            | LExpr_ReadWrite(f, g, tes, es) ->
                    let f'   = self#visit_var f in
                    let g'   = self#visit_var g in
                    let tes' = self#visit_exprs tes in
                    let es'  = self#visit_exprs es in
                    if f == f' && g == g' && tes == tes' && es == es' then x else LExpr_ReadWrite(f, g, tes', es')
            )
        in
        doVisit vis (vis#vlexpr x) aux x


    method virtual visit_stmts : stmt list -> stmt list

    method with_locals : 'a 'b. (ty * ident) list -> ('a -> 'b) -> 'a -> 'b = fun ls f x ->
        vis#enter_scope ls;
        let result = f x in
        vis#leave_scope ();
        result

    method visit_stmt (x: stmt): stmt list =
        let aux (_: #aslVisitor) (x: stmt): stmt =
            (match x with
            | Stmt_VarDeclsNoInit (ty, vs, loc) ->
                    let ty' = self#visit_type ty in
                    let vs' = mapNoCopy (self#visit_lvar) vs in
                    if ty == ty' && vs == vs' then x else Stmt_VarDeclsNoInit (ty', vs', loc)
            | Stmt_VarDecl (ty, v, i, loc) ->
                    let ty' = self#visit_type ty in
                    let v' = self#visit_lvar v in
                    let i' = self#visit_expr i in
                    if ty == ty' && v == v' && i == i'  then x else Stmt_VarDecl (ty', v', i', loc)
            | Stmt_ConstDecl (ty, v, i, loc) ->
                    let ty' = self#visit_type ty in
                    let v' = self#visit_lvar v in
                    let i' = self#visit_expr i in
                    if ty == ty' && v == v' && i == i' then x else Stmt_ConstDecl (ty', v', i', loc)
            | Stmt_Assign (l, r, loc) ->
                    let l' = self#visit_lexpr l in
                    let r' = self#visit_expr r in
                    if l == l' && r == r' then x else Stmt_Assign (l', r', loc)
            | Stmt_TCall (f, tes, args, loc) ->
                    let f'    = self#visit_var f in
                    let tes'  = self#visit_exprs tes in
                    let args' = self#visit_exprs args in
                    if f == f' && tes == tes' && args == args' then x else Stmt_TCall (f', tes', args', loc)
            | Stmt_FunReturn (e, loc) ->
                    let e' = self#visit_expr e in
                    if e == e' then x else Stmt_FunReturn (e', loc)
            | Stmt_ProcReturn (_) -> x
            | Stmt_Assert (e, loc) ->
                    let e' = self#visit_expr e in
                    if e == e' then x else Stmt_Assert (e', loc)
            | Stmt_Unpred (_) -> x
            | Stmt_ConstrainedUnpred(_) -> x
            | Stmt_ImpDef (v, loc) ->
                    let v' = self#visit_var v in
                    if v == v' then x else Stmt_ImpDef (v', loc)
            | Stmt_Undefined (_) -> x
            | Stmt_ExceptionTaken (_) -> x
            | Stmt_Dep_Unpred (_) -> x
            | Stmt_Dep_ImpDef (_, _) -> x
            | Stmt_Dep_Undefined (_) -> x
            | Stmt_See (e, loc) ->
                    let e' = self#visit_expr e in
                    if e == e' then x else Stmt_See (e', loc)
            | Stmt_Throw (v, loc) ->
                    let v' = self#visit_var v in
                    if v == v' then x else Stmt_Throw (v', loc)
            | Stmt_DecodeExecute (i, e, loc) ->
                    let e' = self#visit_expr e in
                    if e == e' then x else Stmt_DecodeExecute (i, e', loc)
            | Stmt_If (c, t, els, e, loc) ->
                    let c'   = self#visit_expr c in
                    let t'   = self#visit_stmts t in
                    let els' = mapNoCopy (self#visit_s_elsif) els in
                    let e'   = self#visit_stmts e in
                    if c == c' && t == t' && els == els' && e == e' then x else Stmt_If (c', t', els', e', loc)
            | Stmt_Case (e, alts, ob, loc) ->
                    let e'    = self#visit_expr e in
                    let alts' = mapNoCopy (self#visit_alt) alts in
                    let ob'   = mapOptionNoCopy (self#visit_stmts) ob in
                    if e == e' && alts == alts' && ob == ob' then x else Stmt_Case (e', alts', ob', loc)
            | Stmt_For (v, f, dir, t, b, loc) ->
                    let v' = self#visit_lvar v in
                    let f' = self#visit_expr f in
                    let t' = self#visit_expr t in
                    let ty_v' = (Type_Constructor(Ident "integer"), v') in
                    let b' = self#with_locals [ty_v'] self#visit_stmts b in
                    if v == v' && f == f' && t == t' && b == b' then x else Stmt_For (v', f', dir, t', b', loc)
            | Stmt_While (c, b, loc) ->
                    let c' = self#visit_expr c in
                    let b' = self#visit_stmts b in
                    if c == c' && b == b' then x else Stmt_While (c', b', loc)
            | Stmt_Repeat (b, c, loc) ->
                    let b' = self#visit_stmts b in
                    let c' = self#visit_expr c in
                    if b == b' && c == c' then x else Stmt_Repeat (b', c', loc)
            | Stmt_Try (b, v, cs, ob, loc) ->
                    let b'  = self#visit_stmts b in
                    let v'  = self#visit_lvar v in
                    let ty_v' = (Type_Constructor(Ident "__Exception"), v') in
                    let cs' = mapNoCopy (self#with_locals [ty_v'] self#visit_catcher) cs in
                    let ob' = mapOptionNoCopy (self#with_locals [ty_v'] self#visit_stmts) ob in
                    if b == b' && v == v' && cs == cs' && ob == ob' then x else Stmt_Try (b', v', cs', ob', loc)

            )
        in
        doVisitList vis (vis#vstmt x) aux x

    method visit_s_elsif (x: s_elsif): s_elsif =
        let aux (_: #aslVisitor) (x: s_elsif): s_elsif =
            (match x with
            | S_Elsif_Cond(c, b) ->
                    let c' = self#visit_expr c in
                    let b' = self#visit_stmts b in
                    if c == c' && b == b' then x else S_Elsif_Cond(c', b')
            )
        in
        doVisit vis (vis#vs_elsif x) aux x

    method visit_alt (x: alt): alt =
        let aux (_: #aslVisitor) (x: alt): alt =
            (match x with
            | Alt_Alt(ps, oc, b) ->
                    let ps' = self#visit_patterns ps in
                    let oc' = mapOptionNoCopy (self#visit_expr) oc in
                    let b' = self#visit_stmts b in
                    if ps == ps' && oc == oc' && b == b' then x else Alt_Alt(ps', oc', b')
            )
        in
        doVisit vis (vis#valt x) aux x

    method visit_catcher (x: catcher): catcher =
        let aux (_: #aslVisitor) (x: catcher): catcher =
            (match x with
            | Catcher_Guarded(c, b) ->
                    let c' = self#visit_expr c in
                    let b' = self#visit_stmts b in
                    if c == c' && b == b' then x else Catcher_Guarded(c', b')
            )
        in
        doVisit vis (vis#vcatcher x) aux x


    method visit_mapfield (x: mapfield): mapfield =
        let aux (_: #aslVisitor) (x: mapfield): mapfield =
            (match x with
            | MapField_Field (v, p) ->
                    let v' = self#visit_var v in
                    let p' = self#visit_pattern p in
                    if v == v' && p == p' then x else MapField_Field (v', p')
            )
        in
        doVisit vis (vis#vmapfield x) aux x

    method visit_sformal (x: sformal): sformal =
        let aux (_: #aslVisitor) (x: sformal): sformal =
            (match x with
            | Formal_In (ty, v) ->
                    let ty' = self#visit_type ty in
                    let v' = self#visit_lvar v in
                    if ty == ty' && v == v' then x else Formal_In (ty', v')
            | Formal_InOut(ty, v) ->
                    let ty' = self#visit_type ty in
                    let v' = self#visit_lvar v in
                    if ty == ty' && v == v' then x else Formal_InOut (ty', v')
            )
        in
        doVisit vis (vis#vsformal x) aux x

    method visit_dpattern (x: decode_pattern): decode_pattern =
        let aux (_: #aslVisitor) (x: decode_pattern): decode_pattern =
            (match x with
            | DecoderPattern_Bits _ -> x
            | DecoderPattern_Mask _ -> x
            | DecoderPattern_Wildcard _ -> x
            | DecoderPattern_Not p ->
                    let p' = self#visit_dpattern p in
                    if p == p' then x else DecoderPattern_Not p'
            )
        in
        doVisit vis (vis#vdpattern x) aux x

    method visit_encoding (x: encoding): encoding =
        let aux (_: #aslVisitor) (x: encoding): encoding =
            (match x with
            | Encoding_Block (nm, iset, fs, op, e, ups, b, loc) ->
                    let e' = self#visit_expr e in
                    let b' = self#visit_stmts b in
                    if e == e' && b == b' then x else Encoding_Block (nm, iset, fs, op, e, ups, b', loc)
            )
        in
        doVisit vis (vis#vencoding x) aux x

    method visit_decode_case (x: decode_case): decode_case =
        let aux (_: #aslVisitor) (x: decode_case): decode_case =
            (match x with
            | DecoderCase_Case (ss, alts, loc) ->
                    let alts' = mapNoCopy (self#visit_decode_alt) alts in
                    if alts == alts' then x else DecoderCase_Case (ss, alts', loc)
            )
        in
        doVisit vis (vis#vdcase x) aux x

    method visit_decode_alt (x: decode_alt): decode_alt =
        let aux (_: #aslVisitor) (x: decode_alt): decode_alt =
            (match x with
            | DecoderAlt_Alt (ps, b) ->
                    let ps' = mapNoCopy (self#visit_dpattern) ps in
                    let b'  = self#visit_decode_body b in
                    if ps == ps' && b == b' then x else
                    DecoderAlt_Alt (ps', b')
            )
        in
        doVisit vis (vis#vdalt x) aux x

    method visit_decode_body (x: decode_body): decode_body =
        let aux (_: #aslVisitor) (x: decode_body): decode_body =
            (match x with
            | DecoderBody_UNPRED   _ -> x
            | DecoderBody_UNALLOC  _ -> x
            | DecoderBody_NOP      _ -> x
            | DecoderBody_Encoding _ -> x
            | DecoderBody_Decoder (fs, c, loc) ->
                    let c' = self#visit_decode_case c in
                    if c == c' then x else DecoderBody_Decoder (fs, c', loc)
            )
        in
        doVisit vis (vis#vdbody x) aux x

    method visit_arg (x: (ty * ident)): (ty * ident) =
        (match x with
        | (ty, v) ->
                let ty' = self#visit_type ty in
                let v'  = self#visit_var v in
                if ty == ty' && v == v' then x else
                (ty', v')
        )

    method visit_args (xs: (ty * ident) list): (ty * ident) list =
            mapNoCopy (self#visit_arg) xs

    method visit_decl (x: declaration): declaration =
        let aux (_: #aslVisitor) (x: declaration): declaration =
            (match x with
            | Decl_BuiltinType (v, loc) ->
                    let v'  = self#visit_var v in
                    if v == v' then x else
                    Decl_BuiltinType (v', loc)
            | Decl_Forward (v, loc) ->
                    let v'  = self#visit_var v in
                    if v == v' then x else
                    Decl_Forward (v', loc)
            | Decl_Record (v, fs, loc) ->
                    let v'  = self#visit_var v in
                    let fs' = self#visit_args fs in
                    if v == v' && fs == fs' then x else
                    Decl_Record (v', fs', loc)
            | Decl_Typedef (v, ty, loc) ->
                    let v'  = self#visit_var v in
                    let ty' = self#visit_type ty in
                    if v == v' && ty == ty' then x else
                    Decl_Typedef (v', ty', loc)
            | Decl_Enum (v, es, loc) ->
                    let v'  = self#visit_var v in
                    let es' = mapNoCopy (self#visit_var) es in
                    if v == v' && es == es' then x else
                    Decl_Enum (v', es', loc)
            | Decl_Var (ty, v, loc) ->
                    let ty' = self#visit_type ty in
                    let v'  = self#visit_var v in
                    if ty == ty' && v == v' then x else
                    Decl_Var (ty', v', loc)
            | Decl_Const (ty, v, e, loc) ->
                    let ty' = self#visit_type ty in
                    let v'  = self#visit_var v in
                    let e'  = self#visit_expr e in
                    if ty == ty' && v == v' && e == e' then x else
                    Decl_Const (ty', v', e', loc)
            | Decl_BuiltinFunction (ty, f, args, loc) ->
                    let ty'   = self#visit_type ty in
                    let f'    = self#visit_var f in
                    let args' = self#visit_args args in
                    if ty == ty' && f == f' && args == args' then x else
                    Decl_BuiltinFunction (ty', f', args', loc)
            | Decl_FunType (ty, f, args, loc) ->
                    let ty'   = self#visit_type ty in
                    let f'    = self#visit_var f in
                    let args' = self#visit_args args in
                    if ty == ty' && f == f' && args == args' then x else
                    Decl_FunType (ty', f', args', loc)
            | Decl_FunDefn (ty, f, args, b, loc) ->
                    let ty'   = self#visit_type ty in
                    let f'    = self#visit_var f in
                    let args' = self#visit_args args in
                    let b'    = self#with_locals args' self#visit_stmts b in
                    if ty == ty' && f == f' && args == args' && b == b' then x else
                    Decl_FunDefn (ty', f', args', b', loc)
            | Decl_ProcType (f, args, loc) ->
                    let f'    = self#visit_var f in
                    let args' = self#visit_args args in
                    if f == f' && args == args' then x else
                    Decl_ProcType (f', args', loc)
            | Decl_ProcDefn (f, args, b, loc) ->
                    let f'    = self#visit_var f in
                    let args' = self#visit_args args in
                    let b'    = self#with_locals args' self#visit_stmts b in
                    if f == f' && args == args' && b == b' then x else
                    Decl_ProcDefn (f', args', b', loc)
            | Decl_VarGetterType (ty, f, loc) ->
                    let ty' = self#visit_type ty in
                    let f'  = self#visit_var f in
                    if ty == ty' && f == f' then x else
                    Decl_VarGetterType (ty', f', loc)
            | Decl_VarGetterDefn (ty, f, b, loc) ->
                    let ty' = self#visit_type ty in
                    let f'  = self#visit_var f in
                    let b'  = self#visit_stmts b in
                    if ty == ty' && f == f' && b == b' then x else
                    Decl_VarGetterDefn (ty', f', b', loc)
            | Decl_ArrayGetterType (ty, f, args, loc) ->
                    let ty'   = self#visit_type ty in
                    let f'    = self#visit_var f in
                    let args' = self#visit_args args in
                    if ty == ty' && f == f' && args == args' then x else
                    Decl_ArrayGetterType (ty', f', args', loc)
            | Decl_ArrayGetterDefn (ty, f, args, b, loc) ->
                    let ty'   = self#visit_type ty in
                    let f'    = self#visit_var f in
                    let args' = self#visit_args args in
                    let b'    = self#with_locals args' self#visit_stmts b in
                    if ty == ty' && f == f' && args == args' && b == b' then x else
                    Decl_ArrayGetterDefn (ty', f', args', b', loc)
            | Decl_VarSetterType (f, ty, v, loc) ->
                    let f'  = self#visit_var f in
                    let ty' = self#visit_type ty in
                    let v'  = self#visit_var v in
                    if f == f' && ty == ty' && v == v' then x else
                    Decl_VarSetterType (f', ty', v', loc)
            | Decl_VarSetterDefn (f, ty, v, b, loc) ->
                    let f'  = self#visit_var f in
                    let ty' = self#visit_type ty in
                    let v'  = self#visit_var v in
                    let b'  = self#with_locals [(ty', v')] self#visit_stmts b in
                    if f == f' && ty == ty' && v == v' && b == b' then x else
                    Decl_VarSetterDefn (f', ty', v', b', loc)
            | Decl_ArraySetterType (f, args, ty, v, loc) ->
                    let f'    = self#visit_var f in
                    let args' = mapNoCopy (self#visit_sformal) args in
                    let ty'   = self#visit_type ty in
                    let v'    = self#visit_var v in
                    if f == f' && args == args' && ty == ty' && v == v' then x else
                    Decl_ArraySetterType (f', args', ty', v', loc)
            | Decl_ArraySetterDefn (f, args, ty, v, b, loc) ->
                    let f'    = self#visit_var f in
                    let args' = mapNoCopy (self#visit_sformal) args in
                    let ty'   = self#visit_type ty in
                    let v'    = self#visit_var v in
                    let lvars = List.map arg_of_sformal args' @ [(ty', v')] in
                    let b'    = self#with_locals lvars self#visit_stmts b in
                    if f == f' && args == args' && ty == ty' && v == v' && b == b' then x else
                    Decl_ArraySetterDefn (f', args', ty', v', b', loc)
            | Decl_InstructionDefn (d, es, opd, c, ex, loc) ->
                    let d'    = self#visit_var d in
                    let es'   = mapNoCopy (self#visit_encoding) es in
                    let lvars = List.concat (List.map args_of_encoding es) in
                    let opd'  = mapOptionNoCopy (self#with_locals lvars self#visit_stmts) opd in
                    let ex'   = self#with_locals lvars self#visit_stmts ex in
                    if d == d' && es == es' && opd == opd' && ex == ex' then x else
                    Decl_InstructionDefn (d', es', opd', c, ex', loc)
            | Decl_DecoderDefn (d, dc, loc) ->
                    let d'  = self#visit_var d in
                    let dc' = self#visit_decode_case dc in
                    if d == d' && dc == dc' then x else
                    Decl_DecoderDefn (d', dc', loc)
            | Decl_Operator1 (op, vs, loc) ->
                    let vs' = mapNoCopy (self#visit_var) vs in
                    if vs == vs' then x else
                    Decl_Operator1 (op, vs', loc)
            | Decl_Operator2 (op, vs, loc) ->
                    let vs' = mapNoCopy (self#visit_var) vs in
                    if vs == vs' then x else
                    Decl_Operator2 (op, vs', loc)
            | Decl_NewEventDefn(v, args, loc) ->
                    let v'    = self#visit_var v in
                    let args' = self#visit_args args in
                    if v == v' && args == args' then x else
                    Decl_NewEventDefn(v', args', loc)
            | Decl_EventClause(v, b, loc) ->
                    let v'  = self#visit_var v in
                    let b'  = self#visit_stmts b in
                    if v == v' && b == b' then x else
                    Decl_EventClause(v', b', loc)
            | Decl_NewMapDefn(ty, v, args, b, loc) ->
                    let ty'   = self#visit_type ty in
                    let v'    = self#visit_var v in
                    let args' = self#visit_args args in
                    let b'    = self#with_locals args' self#visit_stmts b in
                    if v == v' && args == args' && b == b' then x else
                    Decl_NewMapDefn(ty', v', args', b', loc)
            | Decl_MapClause(v, fs, oc, b, loc) ->
                    let v'  = self#visit_var v in
                    let fs' = mapNoCopy (self#visit_mapfield) fs in
                    let oc' = mapOptionNoCopy (self#visit_expr) oc in
                    let b'  = self#visit_stmts b in
                    if v == v' && fs == fs' && oc == oc' && b == b' then x else
                    Decl_MapClause(v', fs', oc', b', loc)
            | Decl_Config(ty, v, e, loc) ->
                    let ty' = self#visit_type ty in
                    let v'  = self#visit_var v in
                    let e'  = self#visit_expr e in
                    if ty == ty' && v == v' && e == e' then x else
                    Decl_Config(ty', v', e', loc)
            )

        in
        doVisit vis (vis#vdecl x) aux x
end

class aslForwardsVisitor (vis: #aslVisitor) = object(self)
    inherit aslTreeVisitor vis

    method visit_stmts (xs: stmt list): stmt list =
        vis#enter_scope [];
        let stmts' = List.concat_map (self#visit_stmt) xs in
        vis#leave_scope ();
        stmts'
end

(** visit statement lists in a backwards order.
    i.e., enter_scope is called before the final statement in a block and
    exit_scope is called after the initial statement. *)
class aslBackwardsVisitor (vis: #aslVisitor) = object(self)
    inherit aslTreeVisitor vis

    method visit_stmts (xs: stmt list): stmt list =
        vis#enter_scope [];
        (* reverse resultant statements as blocks, to avoid reversing
           lists returned by the visitAction. *)
        let stmts' = List.rev @@ List.map (self#visit_stmt) (List.rev xs) in
        vis#leave_scope ();
        List.concat stmts'
end


(* convenience methods to visit with the ordinary aslForwardsVisitor. *)

let visit_exprs (vis: #aslVisitor) : expr list -> expr list = (new aslForwardsVisitor vis)#visit_exprs

let visit_var (vis: #aslVisitor) : ident -> ident = (new aslForwardsVisitor vis)#visit_var

let visit_lvar (vis: #aslVisitor) : ident -> ident = (new aslForwardsVisitor vis)#visit_lvar

let visit_e_elsif (vis: #aslVisitor) : e_elsif -> e_elsif = (new aslForwardsVisitor vis)#visit_e_elsif

let visit_slice (vis: #aslVisitor) : slice -> slice = (new aslForwardsVisitor vis)#visit_slice

let visit_patterns (vis: #aslVisitor) : pattern list -> pattern list = (new aslForwardsVisitor vis)#visit_patterns

let visit_pattern (vis: #aslVisitor) : pattern -> pattern = (new aslForwardsVisitor vis)#visit_pattern

let visit_expr (vis: #aslVisitor) : expr -> expr = (new aslForwardsVisitor vis)#visit_expr

let visit_types (vis: #aslVisitor) : ty list -> ty list = (new aslForwardsVisitor vis)#visit_types

let visit_type (vis: #aslVisitor) : ty -> ty = (new aslForwardsVisitor vis)#visit_type

let visit_lexprs (vis: #aslVisitor) : lexpr list -> lexpr list = (new aslForwardsVisitor vis)#visit_lexprs

let visit_lexpr (vis: #aslVisitor) : lexpr -> lexpr = (new aslForwardsVisitor vis)#visit_lexpr

let visit_stmts (vis: #aslVisitor) : stmt list -> stmt list = (new aslForwardsVisitor vis)#visit_stmts

let visit_stmt (vis: #aslVisitor) : stmt -> stmt list = (new aslForwardsVisitor vis)#visit_stmt

let visit_s_elsif (vis: #aslVisitor) : s_elsif -> s_elsif = (new aslForwardsVisitor vis)#visit_s_elsif

let visit_alt (vis: #aslVisitor) : alt -> alt = (new aslForwardsVisitor vis)#visit_alt

let visit_catcher (vis: #aslVisitor) : catcher -> catcher = (new aslForwardsVisitor vis)#visit_catcher

let visit_mapfield (vis: #aslVisitor) : mapfield -> mapfield = (new aslForwardsVisitor vis)#visit_mapfield

let visit_sformal (vis: #aslVisitor) : sformal -> sformal = (new aslForwardsVisitor vis)#visit_sformal

let visit_dpattern (vis: #aslVisitor) : decode_pattern -> decode_pattern = (new aslForwardsVisitor vis)#visit_dpattern

let visit_encoding (vis: #aslVisitor) : encoding -> encoding = (new aslForwardsVisitor vis)#visit_encoding

let visit_decode_case (vis: #aslVisitor) : decode_case -> decode_case = (new aslForwardsVisitor vis)#visit_decode_case

let visit_decode_alt (vis: #aslVisitor) : decode_alt -> decode_alt = (new aslForwardsVisitor vis)#visit_decode_alt

let visit_decode_body (vis: #aslVisitor) : decode_body -> decode_body = (new aslForwardsVisitor vis)#visit_decode_body

let visit_arg (vis: #aslVisitor) : (ty * ident) -> (ty * ident) = (new aslForwardsVisitor vis)#visit_arg

let visit_args (vis: #aslVisitor) : (ty * ident) list -> (ty * ident) list = (new aslForwardsVisitor vis)#visit_args

let visit_decl (vis: #aslVisitor) : declaration -> declaration = (new aslForwardsVisitor vis)#visit_decl

let visit_stmt_single (vis: #aslVisitor) : stmt -> stmt =
    fun s -> match visit_stmt vis s with
    | [x] -> x
    | _ -> failwith "visit_stmt_single requires exactly one returned statement"


(****************************************************************)
(** {2 nopAslVisitor class}                                     *)
(****************************************************************)

(** The nopAslVisitor class defines a visitor that recursively
    visits the entire tree making no change.
    In practice, all uses of the visitor framework are based on defining
    a subclass of this type.
 *)

class nopAslVisitor : aslVisitor = object

    method vvar      (_: ident)          = DoChildren
    method ve_elsif  (_: e_elsif)        = DoChildren
    method vslice    (_: slice)          = DoChildren
    method vpattern  (_: pattern)        = DoChildren
    method vexpr     (_: expr)           = DoChildren
    method vtype     (_: ty)             = DoChildren
    method vlvar     (_: ident)          = DoChildren
    method vlexpr    (_: lexpr)          = DoChildren
    method vstmt     (_: stmt)           = DoChildren
    method vs_elsif  (_: s_elsif)        = DoChildren
    method valt      (_: alt)            = DoChildren
    method vcatcher  (_: catcher)        = DoChildren
    method vmapfield (_: mapfield)       = DoChildren
    method vsformal  (_: sformal)        = DoChildren
    method vdpattern (_: decode_pattern) = DoChildren
    method vencoding (_: encoding)       = DoChildren
    method vdcase    (_: decode_case)    = DoChildren
    method vdalt     (_: decode_alt)     = DoChildren
    method vdbody    (_: decode_body)    = DoChildren
    method vdecl     (_: declaration)    = DoChildren

    method enter_scope _ = ()
    method leave_scope _ = ()
end

(****************************************************************
 * End
 ****************************************************************)
