module AST = Asl_ast

open AST
open Value
open Asl_utils
open Primops

type sym =
  | Val of value
  | Exp of expr

type 'a sym_pattern =
  | SymPat_LitInt of bitsLit
  | SymPat_LitHex of bitsLit
  | SymPat_LitBits of bitsLit
  | SymPat_LitMask of bitsLit
  | SymPat_Const of ident
  | SymPat_Wildcard
  | SymPat_Tuple of pattern list
  | SymPat_Set of pattern list
  | SymPat_Range of 'a * 'a
  | SymPat_Single of 'a

type 'a sym_expr =
  | SymExpr_If of 'a * 'a * ('a * 'a) list * 'a
  | SymExpr_Binop of 'a * binop * 'a
  | SymExpr_Unop of unop * 'a
  | SymExpr_Field of 'a * ident
  | SymExpr_Fields of 'a * ident list
  | SymExpr_Slices of 'a * ('a * 'a) list
  | SymExpr_In of 'a * 'a sym_pattern
  | SymExpr_Var of ident
  | SymExpr_Parens of 'a
  | SymExpr_Tuple of 'a list
  | SymExpr_Unknown of ty
  | SymExpr_ImpDef of ty * string option
  | SymExpr_TApply of ident * 'a list * 'a list
  | SymExpr_Array of 'a * 'a
  | SymExpr_LitInt of string
  | SymExpr_LitHex of string
  | SymExpr_LitReal of string
  | SymExpr_LitBits of string
  | SymExpr_LitMask of string
  | SymExpr_LitString of string


type sym' =
  | Val' of value
  | Exp' of ty * (sym' sym_expr)

let is_val (x: AST.expr): bool =
    (match x with
    | Expr_LitInt _ -> true
    | Expr_LitBits _ -> true
    | Expr_LitReal _ -> true
    | Expr_LitString _ -> true
    | Expr_Tuple _ -> true
    | x -> false
    )

let rec val_expr (v: Value.value): AST.expr =
  match v with
  | VBool b -> Expr_Var(if b then Ident "TRUE" else Ident "FALSE")
  | VEnum (id, n) -> Expr_LitInt(string_of_int n)
  | VInt n -> Expr_LitInt(Z.to_string n)
  | VReal n -> Expr_LitReal(Q.to_string n)
  | VBits {n; v} -> Expr_LitBits(Z.format ("%0" ^ string_of_int n ^ "b") v)
  | VString s -> Expr_LitString(s)
  | VTuple vs -> Expr_Tuple(List.map val_expr vs)
  | _ -> failwith @@ "Casting unhandled value type to expression: " ^ pp_value v

let rec val_initialised (v: value): bool =
  match v with
  | VUninitialized _ -> false
  | VRecord bs -> Bindings.for_all (fun _ -> val_initialised) bs
  | VTuple vs -> List.for_all val_initialised vs
  | VArray (vs, _) -> Primops.ImmutableArray.for_all (fun _ -> val_initialised) vs
  | _ -> true

let rec expr_to_lexpr (e: expr): lexpr =
  match e with
  | Expr_Var x -> LExpr_Var x
  | Expr_Field (e,f) -> LExpr_Field (expr_to_lexpr e, f)
  | Expr_Fields (e,fs) -> LExpr_Fields (expr_to_lexpr e, fs)
  | Expr_Slices (e,ss) -> LExpr_Slices (expr_to_lexpr e, ss)
  | Expr_Tuple es -> LExpr_Tuple (List.map expr_to_lexpr es)
  | Expr_Array (e,i) -> LExpr_Array (expr_to_lexpr e, i)
  | Expr_TApply (FIdent (nm, n), tes, es) when Utils.endswith nm ".read" ->
    (match String.split_on_char '.' nm with
    | [nm'; "read"] -> LExpr_Write (FIdent (nm' ^ ".write", n), tes, es)
    | _ -> raise (EvalError (Unknown, "expr_to_lexpr: cannot derive lexpr for " ^ pp_expr e)))
  | _ -> raise (EvalError (Unknown, "unexpected expression in expr_to_lexpr coercion: " ^ pp_expr e))

let int_of_expr (e: expr): int =
  match e with
  | Expr_LitInt(i) ->    Z.to_int (Z.of_string i)
  | Expr_LitHex(i) ->    Z.to_int (Z.of_string_base 16 (drop_chars i '_'))
  | _ -> failwith @@ "int_of_expr: cannot coerce to int " ^ pp_expr e

let sym_of_int (n: int): sym =
  Val (VInt (Z.of_int n))

let expr_of_int n =
  Expr_LitInt (string_of_int n)

(** Coerces the expression to a symbolic expression, converting
    literal expressions into values. *)
let sym_of_expr (e: expr): sym =
  match e with
  | Expr_LitInt(i) ->    (Val (from_intLit i))
  | Expr_LitHex(i) ->    (Val (from_hexLit i))
  | Expr_LitReal(r) ->   (Val (from_realLit r))
  | Expr_LitBits(b) ->   (Val (from_bitsLit b))
  | Expr_LitMask(b) ->   (Val (from_maskLit b))
  | Expr_LitString(s) -> (Val (from_stringLit s))
  | _ -> Exp e

let rec lexpr_to_expr (loc: l) (x: lexpr): expr =
  (match x with
  | LExpr_Var(id) -> Expr_Var(id)
  | LExpr_Field(l,f) -> Expr_Field(lexpr_to_expr loc l,f)
  | LExpr_Array(l,i) -> Expr_Array(lexpr_to_expr loc l,i)
  | _ -> raise (EvalError (Unknown, "unexpected expression in lexpr_to_expr coercion: " ^ pp_lexpr x)))

let filter_uninit (v: value option): value option =
  match v with
  | Some (VUninitialized _) -> None
  | _ -> v

let sym_value_unsafe (x: sym): value =
  match x with
  | Val v -> v
  | Exp e -> failwith ("sym_value_unsafe: required value but got " ^ pp_expr e)

let sym_expr (x: sym): expr =
  match x with
    | Val v -> val_expr v
    | Exp e -> e

let sym_pair_has_exp (pair: sym * sym): bool =
  match pair with
  | Exp _, _ -> true
  | _, Exp _ -> true
  | _ -> false

let sym_initialised (x: sym): sym option =
  match x with
  | Val v -> if val_initialised v then Some x else None
  | Exp _ -> Some x

(** Constructs a sym of a tuple when given a sym for each element in the tuple.
    Result is a Val iff all components are Val.  *)
let rec sym_tuple (syms: sym list): sym =
  match syms with
  | [] -> Val (VTuple [])
  | Val v::rest ->
    (match sym_tuple rest with
    | Val (VTuple vs) -> Val (VTuple (v::vs))
    | Exp (Expr_Tuple es) -> Exp (Expr_Tuple (val_expr v :: es))
    | _ -> failwith "unreachable: sym_tuple should only return tuple in values or expressions.")
  | Exp e::_ -> Exp (Expr_Tuple (List.map sym_expr syms))

let pp_sym (rs: sym): string =
    match rs with
    | Val v -> Printf.sprintf "Val(%s)" (pp_value v)
    | Exp e -> Printf.sprintf "Exp(%s)" (pp_expr e)

let sym_of_tuple (loc: AST.l) (v: sym): sym list  =
  match v with
  | Val (VTuple vs) -> (List.map (fun v -> Val v) vs)
  | Exp (Expr_Tuple vs) -> (List.map (fun v -> Exp v) vs)
  | _ -> raise (EvalError (loc, "tuple expected. Got "^ pp_sym v))

(* Types *)

let type_bool = Type_Constructor(Ident "boolean")
let type_unknown = Type_Constructor(Ident "unknown")

(* Primitives *)

let prim_binop_targs (f: string) (loc: l) (x: sym) (y: sym): value list =
  let fail () = raise (EvalError (loc,
    "cannot infer type arguments for primitive binop: " ^
    f ^ " " ^ pp_sym x ^ ", " ^ pp_sym y))
  in
  match f with
  | "eq_bits" ->
    (match x,y with
    | Val (VBits {n=n; _}), _
    | _, Val (VBits {n=n; _}) -> [VInt (Z.of_int n)]
    | _ -> fail())
  | "in_mask" ->
    (match x,y with
    | Val (VBits {n=n; _}), _
    | _, Val (VMask {n=n; _}) -> [VInt (Z.of_int n)]
    | _ -> fail ())
  | _ -> [] (* assume other binops have no type arguments. *)

(** Apply a primitive operation to two arguments
  *)
let prim_binop (f: string) (loc: AST.l) (x: sym) (y: sym) : sym  =
  let targs = prim_binop_targs f loc x y in
  (match (x,y) with
  | (Val x,Val y) ->
      (match eval_prim f targs [x;y] with
      | Some v -> Val v
      | None -> raise (EvalError (loc, "Unknown primitive operation: "^ f)))
  | (x,y) -> Exp (Expr_TApply(
      FIdent(f,0),
      List.map val_expr targs,
      [sym_expr x; sym_expr y])))

(** Coerces sym to value but DOES NOT return correct uninitialised
    structures with types. *)
let sym_val_or_uninit_unsafe (x: sym): value =
  match x with
  | Val v -> v
  | Exp e -> VUninitialized (Type_OfExpr e)

let expr_prim f tes es =
  Expr_TApply (f, tes, es)

let expr_prim' f = expr_prim (FIdent (f, 0))

let sym_prim (f: ident) (tes: sym list) (es: sym list): sym =
  let tes_vals = List.map sym_val_or_uninit_unsafe tes
  and es_vals = List.map sym_val_or_uninit_unsafe es in
  match eval_prim (name_of_FIdent f) tes_vals es_vals with
  | None -> Exp (expr_prim f (List.map sym_expr tes) (List.map sym_expr es))
  | Some v -> Val (v)

let sym_true     = Val (from_bool true)
let sym_false    = Val (from_bool false)
let expr_true    = Expr_Var (Ident "TRUE")
let expr_false   = Expr_Var (Ident "FALSE")
let sym_zeros n  = Val (VBits (prim_zeros_bits (Z.of_int n)))

let sym_eq_int   = prim_binop "eq_int"
let sym_add_int  = prim_binop "add_int"
let sym_sub_int  = prim_binop "sub_int"
let sym_le_int   = prim_binop "le_int"

let sym_eq_bits  = prim_binop "eq_bits"
let sym_inmask   = prim_binop "in_mask"

let sym_and_bool loc (x: sym) (y: sym) =
  match (x,y) with
  | (Val x', y') -> if to_bool loc x' then y' else sym_false
  | (x', Val y') -> if to_bool loc y' then x' else sym_false
  | _ -> Exp (Expr_TApply(FIdent("and_bool",0), [], [sym_expr x;sym_expr y]))

let sym_eq (loc: AST.l) (x: sym) (y: sym): sym =
  (match (x,y) with
  | (Val x,Val y) -> Val (from_bool (eval_eq loc x y))
  | (Exp _,Val v) | (Val v, Exp _) ->
      (match v with
      | VBits _ -> sym_eq_bits loc x y
      | VInt _ -> sym_eq_int loc x y
      | _ -> failwith "sym_eq: unknown value type")
  | (_,_) -> failwith "sym_eq: insufficient info to resolve type")

(*** Symbolic Bitvector Operations ***)

(** Append two bitvector symbols and explicitly provide their widths *)
let sym_append_bits (loc: l) (xw: int) (yw: int) (x: sym) (y: sym): sym =
  (match (x,y) with
  | (Val (VBits {n=0; _}), y) -> y
  | (x, Val (VBits {n=0; _})) -> x
  | (Val (VBits x),Val (VBits y)) -> Val (VBits (prim_append_bits x y))

  (* special case: if y is already an append operation, fuse x into its y's left argument. *)
  | (Val (VBits v), Exp (Expr_TApply (FIdent ("append_bits", 0),
      [Expr_LitInt lw; rw], [Expr_LitBits l;r]))) ->

    let l = (to_bits Unknown (from_bitsLit l)) in
    let l' = val_expr (VBits (prim_append_bits v l)) in
    Exp (expr_prim' "append_bits"
      [expr_of_int (xw + int_of_string lw); rw]
      [l'; r])

  | (x,y) ->
    Exp (expr_prim' "append_bits" [expr_of_int xw; expr_of_int yw] [sym_expr x;sym_expr y])
  )

(* WARNING: incorrect type arguments passed to append_bits but sufficient for evaluation
   of primitive with eval_prim. *)
let sym_append_bits_unsafe loc x y = sym_append_bits loc (-1) (-1) x y

let expr_of_int n = Expr_LitInt (string_of_int n)

let sym_replicate (xw: int) (x: sym) (n: int): sym =
  match n with
  | _ when n < 0 -> failwith @@ "sym_replicate: negative replicate count"
  | 0 -> Val (from_bitsLit "")
  | 1 -> x
  | _ ->
    match x with
    | Val (VBits b) -> Val (VBits (prim_replicate_bits b (Z.of_int n)))
    | Val _ -> failwith @@ "sym_replicate: invalid replicate value " ^ pp_sym x
    | Exp e -> Exp (
      Expr_TApply (
        FIdent ("replicate_bits", 0),
        [expr_of_int xw; expr_of_int n],
        [e; expr_of_int n]))

(** Extract a slice from a symbolic bitvector given known bounds.
    Applies optimisations to collapse consecutive slice operations and
    distributes slices across bitvector append operations.
  *)
let rec sym_slice (loc: l) (x: sym) (lo: int) (wd: int): sym =
  let int_expr i = Expr_LitInt (string_of_int i) in
  match x with
  | Val v -> Val (extract_bits'' loc v (VInt (Z.of_int lo)) (VInt (Z.of_int wd)))
  | Exp e ->
    if wd = 0 then Val (VBits empty_bits) else
    let slice_expr =
      (Expr_Slices (e, [Slice_LoWd (int_expr lo, int_expr wd)])) in
    (match e with
    | (Expr_Slices (e', [Slice_LoWd (Expr_LitInt l',_)])) ->
        let l2 = int_of_string l' in
        sym_slice loc (sym_of_expr e') (l2 + lo) wd
    | (Expr_TApply (
        FIdent (("ZeroExtend" | "SignExtend") as ext_type, 0),
        [Expr_LitInt t1; Expr_LitInt t2],
        [x;_])) ->
      let t1 = int_of_string t1 in (* old width *)
      let t2 = int_of_string t2 in (* extended width *)
      let ext_wd = (t2 - t1) in (* width of extend bits *)
      let ext_bit =
        match ext_type with
        | "ZeroExtend" -> Val (from_bitsLit "0")
        | "SignExtend" -> sym_slice loc (sym_of_expr x) (t1-1) 1
        | _ -> assert false
      in
      let ext = sym_replicate 1 ext_bit ext_wd in
      sym_slice loc
        (sym_append_bits loc ext_wd t1 ext (sym_of_expr x))
        lo wd
    | (Expr_TApply (FIdent ("append_bits", 0), [Expr_LitInt t1; Expr_LitInt t2], [x1; x2])) ->
      let t2 = int_of_string t2 in
      if t2 < 0 then
        (* don't statically know the widths of append, don't optimise *)
        Exp slice_expr
      else if (lo >= t2) then
        (* slice is entirely within upper part (i.e. significant bits). *)
        sym_slice loc (sym_of_expr x1) (lo - t2) wd
      else if (lo + wd <= t2) then
        (* entirely within lower part. *)
        sym_slice loc (sym_of_expr x2) lo wd
      else
        (* getting bits from both *)
        let w2 = t2 - lo in
        let w1 = wd - w2 in
        let x2' = sym_slice loc (sym_of_expr x2) lo w2 in
        let x1' = sym_slice loc (sym_of_expr x1) 0 w1 in
        sym_append_bits loc  w1 w2 x1' x2'
    | _ -> Exp slice_expr)


(** Wrapper around sym_slice to handle cases of symbolic slice bounds *)
let sym_extract_bits loc v i w =
  match ( i, w) with
  | (Val i', Val w') ->
      let i' = to_int loc i' in
      let w' = to_int loc w' in
      sym_slice loc v i' w'
  | _ -> Exp (Expr_Slices (sym_expr v, [Slice_LoWd (sym_expr i, sym_expr w)]))

let expr_zeros n =
  Expr_LitBits (String.init n (fun _ -> '0'))

let val_zeros n =
  VBits {n=n; v=Z.zero}

let sym_zero_extend num_zeros old_width e =
  match sym_append_bits Unknown num_zeros old_width (Val (val_zeros num_zeros)) e with
  | Val v -> Val v
  | Exp _ ->
      let n' = expr_of_int (num_zeros + old_width) in
      Exp (expr_prim' "ZeroExtend" [expr_of_int old_width; n'] [sym_expr e; n'])

let sym_sign_extend num_zeros old_width (e: sym): sym =
  let sign = sym_slice Unknown e (old_width-1) 1 in
  let rep = sym_replicate 1 sign num_zeros in
  match sym_append_bits Unknown num_zeros old_width rep e with
  | Val v -> Val v
  | Exp _ ->
      let n' = expr_of_int (num_zeros + old_width) in
      Exp (expr_prim' "SignExtend" [expr_of_int old_width; n'] [sym_expr e; n'])

(** Overwrite bits from position lo up to (lo+wd) exclusive of old with the value v.
    Needs to know the widths of both old and v to perform the operation.
    Assumes width of v is equal to wd.
  *)
let sym_insert_bits loc (old_width: int) (old: sym) (lo: sym) (wd: sym) (v: sym): sym =
  match (old, lo, wd, v) with
  | (Val old', Val i', Val w', Val v') -> Val (insert_bits loc old' i' w' v')
  | (_, Val lo', Val wd', _) ->
      let lo = to_int loc lo' in
      let wd = to_int loc wd' in
      let up = lo + wd in
      if old_width <= up then
        (* Overwriting the top bits of old *)
        sym_append_bits loc wd lo v (sym_slice loc old 0 lo)
      else
        sym_append_bits loc (old_width - up) up (sym_slice loc old up (old_width - up))
          (sym_append_bits loc wd lo v (sym_slice loc old 0 lo))
  | _ ->
      failwith "sym_insert_bits" (* difficult because we need to know widths of each expression. *)

(** Append a list of bitvectors together
    TODO: Will inject invalid widths due to unsafe sym_append_bits call.
 *)
let sym_concat (loc: AST.l) (xs: sym list): sym =
  match xs with
  | [] -> Val (VBits empty_bits)
  | x::xs -> List.fold_left (sym_append_bits_unsafe loc) x xs

let sym_prim_simplify (name: string) (tes: sym list) (es: sym list): sym option =

  let vint_eq cmp = function
    | VInt x when Z.equal cmp x -> true
    | _ -> false in

  let [@warning "-26"] is_zero = vint_eq Z.zero
  and [@warning "-26"] is_one = vint_eq Z.one in

  let is_zero_bits = function
    | (VBits {n = _; v = v}) -> Z.equal Z.zero v
    | _ -> false in

  (match (name, tes, es) with
  | ("add_int",     _,                [Val x1; x2])       when is_zero x1 -> Some x2
  | ("add_int",     _,                [x1; Val x2])       when is_zero x2 -> Some x1
  | ("sub_int",     _,                [x1; Val x2])       when is_zero x2 -> Some x1
  | ("mul_int",     _,                [Val x1; x2])       when is_one x1 -> Some x2
  | ("mul_int",     _,                [x1; Val x2])       when is_one x2 -> Some x1

  | ("append_bits", [Val t1; _],      [_; x2])            when is_zero t1 -> Some x2
  | ("append_bits", [_; Val t2],      [x1; _])            when is_zero t2 -> Some x1
  | ("or_bits",     _,                [Val x1; x2])       when is_zero_bits x1 -> Some x2
  | ("or_bits",     _,                [x1; Val x2])       when is_zero_bits x2 -> Some x1
  | _ -> None)

let rec val_type (v: value): ty =
  let unsupported () = failwith @@ "val_type unsupported: " ^ pp_value v in
  match v with
  | VBool _ -> type_builtin "boolean"
  | VEnum (ident, _) -> Type_Constructor ident
  | VInt _ -> type_builtin "integer"
  | VReal _ -> type_builtin "real"
  | VBits {n=n; _} -> type_bits (string_of_int n)
  | VMask mask -> unsupported ()
  | VString _ -> type_builtin "string"
  | VExc _ -> type_builtin "__Exception"
  | VTuple vs -> Type_Tuple (List.map val_type vs)
  | VRecord (_) -> unsupported ()
  | VArray (arr, def) -> unsupported ()
  | VRAM _ -> type_builtin "__RAM"
  | VUninitialized ty -> ty

let sym_type =
  function
  | Val v -> val_type v
  | Exp e -> Type_OfExpr e (* FIXME: add type annotation to sym Exp constructor. *)

let stmt_loc (s: stmt): l =
  match s with
  | Stmt_VarDeclsNoInit (_, _, l) -> l
  | Stmt_VarDecl (_, _, _, l) -> l
  | Stmt_ConstDecl (_, _, _, l) -> l
  | Stmt_Assign (_, _, l) -> l
  | Stmt_FunReturn (_, l) -> l
  | Stmt_ProcReturn (l) -> l
  | Stmt_Assert (_, l) -> l
  | Stmt_Unpred (l) -> l
  | Stmt_ConstrainedUnpred (l) -> l
  | Stmt_ImpDef (_, l) -> l
  | Stmt_Undefined (l) -> l
  | Stmt_ExceptionTaken (l) -> l
  | Stmt_Dep_Unpred (l) -> l
  | Stmt_Dep_ImpDef (_, l) -> l
  | Stmt_Dep_Undefined (l) -> l
  | Stmt_See (_, l) -> l
  | Stmt_Throw (_, l) -> l
  | Stmt_DecodeExecute (_, _, l) -> l
  | Stmt_TCall (_, _, _, l) -> l
  | Stmt_If (_, _, _, _, l) -> l
  | Stmt_Case (_, _, _, l) -> l
  | Stmt_For (_, _, _, _, _, l) -> l
  | Stmt_While (_, _, l) -> l
  | Stmt_Repeat (_, _, l) -> l
  | Stmt_Try (_, _, _, _, l) -> l

(** Structure to represent a chain of reference expressions *)

type access_chain =
  | Field of ident
  | Index of value

let pp_access_chain =
  function
  | Field id -> "Field " ^ pprint_ident id
  | Index v -> "Index " ^ pp_value v

let pp_access_chain_list = Utils.pp_list pp_access_chain

(* note: for all access_chain lists below, they are ordered with the first
   elements being the inner-most accessor.

   for example:
   a[0][1][2] --> [Index 0; Index 1; Index 2]
   *)

(** "get_access_chain loc v a" returns the reference defined by "a"
    inside the structure "v".  *)
let rec get_access_chain (loc: l) (v: value) (a: access_chain list) : value =
  (match a with
  | (Field f)::a -> (get_access_chain loc (get_field loc v f) a)
  | (Index i)::a -> (get_access_chain loc (get_array loc v i) a)
  | [] -> v)

(** "set_access_chain loc v a r" sets the reference defined by "a"
    in the structure "v" to be "r". *)
let rec set_access_chain (loc: l) (v: value) (a: access_chain list) (r: value): value =
  (match a with
  | (Field f)::a -> set_field loc v f (set_access_chain loc (get_field loc v f) a r)
  | (Index i)::a -> set_array loc v i (set_access_chain loc (get_array loc v i) a r)
  | [] -> r)

(** Returns an lexpr for accessing the given reference within the given lexpr. *)
let rec lexpr_access_chain (x: lexpr) (a: access_chain list): lexpr =
  (match a with
  | (Field f)::a -> lexpr_access_chain (LExpr_Field(x,f)) a
  | (Index i)::a -> lexpr_access_chain (LExpr_Array(x,val_expr i)) a
  | [] -> x)

(** Returns an expr for accessing the given reference within the given expr. *)
let rec expr_access_chain (x: expr) (a: access_chain list): expr =
  (match a with
  | (Field f)::a -> expr_access_chain (Expr_Field(x,f)) a
  | (Index i)::a -> expr_access_chain (Expr_Array(x,val_expr i)) a
  | [] -> x)
