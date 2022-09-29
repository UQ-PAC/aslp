(****************************************************************
 * ASL symbolic evaluator
 ****************************************************************)

module AST = Asl_ast

open AST
open Primops
open Asl_utils
open Abstract_interface

exception SymbolicError of (AST.l * string)

module Test : Value = struct 
  type value =
    | VScalar of (Value.value)
    | VExpr of (AST.ty * AST.expr)
    | VTuple of value list
    | VRecord of value Bindings.t
    | VArray of (value ImmutableArray.t * value)

  let rec from_value v =
    match v with
    | Value.VBool _ 
    | Value.VEnum _
    | Value.VInt _
    | Value.VReal _
    | Value.VBits _
    | Value.VMask _
    | Value.VString _
    | Value.VExc _ 
    | Value.VRAM _
    | Value.VUninitialized -> VScalar v
    | Value.VTuple vs -> VTuple (List.map from_value vs)
    | Value.VRecord vs -> VRecord (Bindings.map from_value vs)
    | Value.VArray (vs,v) -> VArray (ImmutableArray.map from_value vs, from_value v)

  let rec pp_value (x: value): string =
    match x with
    | VScalar v -> Value.pp_value v
    | VExpr (ty,e) -> pp_expr e ^ " :: " ^ pp_type ty
    | VTuple  vs -> "(" ^ String.concat ", " (List.map pp_value vs) ^ ")"
    | VRecord fs ->
        let fs' = List.map (fun (f, v) -> "."^ AST.pprint_ident f ^" = "^ pp_value v) (Bindings.bindings fs)
        in
        "{" ^ String.concat ", " fs' ^ "}"
    | VArray (a, _) ->
        let vs = List.map (fun (i, v) -> string_of_int i ^":"^ pp_value v) (ImmutableArray.bindings a) in
        "[" ^ String.concat ", " vs ^ "]"

  let val_expr (loc: l) (v: Value.value): AST.expr =
    match v with
    | Value.VBool b -> Expr_Var(if b then Ident "TRUE" else Ident "FALSE")
    | Value.VEnum (id, n) -> Expr_LitInt(string_of_int n)
    | Value.VInt n -> Expr_LitInt(Z.to_string n)
    | Value.VReal n -> Expr_LitReal(Q.to_string n)
    | Value.VBits {n; v} -> Expr_LitBits(Z.format ("%0" ^ string_of_int n ^ "b") v)
    | Value.VString s -> Expr_LitString(s)
    | _ -> raise (SymbolicError (loc, "Casting unhandled value type to expression: " ^ Value.pp_value v))

  let value_expr (loc: l) (v: value): AST.expr =
    match v with
    | VScalar v -> val_expr loc v
    | VExpr (_,e) -> e
    | _ -> raise (SymbolicError (loc, "Casting unhandled value type to expression: " ^ pp_value v))

  (* Value Constructors *)
  let from_bool b      = from_value (VBool b)
  let from_int i       = from_value (VInt (Z.of_int i))
  let from_intLit s    = from_value (Value.from_intLit s)
  let from_hexLit s    = from_value (Value.from_hexLit s)
  let from_realLit s   = from_value (Value.from_realLit s)
  let from_bitsLit s   = from_value (Value.from_bitsLit s)
  let from_maskLit s   = from_value (Value.from_maskLit s)
  let from_stringLit s = from_value (Value.from_stringLit s)
  let from_enum e i    = from_value (Value.VEnum (e, i))
  let from_exc loc e   = from_value (Value.VExc (loc, e))
  let from_tuple l     = VTuple l

  (* Value Destructors *)
  let to_scalar loc x =
    match x with
    | VScalar s -> s
    | _ -> raise (SymbolicError (loc, "scalar expected. Got "^ pp_value x))

  let to_string loc x = Value.to_string loc (to_scalar loc x)
  let to_exc loc x = Value.to_exc loc (to_scalar loc x)
  let to_tuple loc v  = 
    match v with
    | VTuple vs -> vs
    | _ -> raise (Value.EvalError (loc, "tuple expected. Got " ^ pp_value v))

  let apply n tes es = Expr_TApply(FIdent(n,0), tes, es)

  (* Unit *)
  let vunit     = VTuple []
  let is_unit v = match v with VTuple [] -> true | _ -> false

  (* Bool *)
  let type_bool = Type_Constructor(Ident "boolean")
  let vbool b = VScalar (Value.VBool b)
  let ebool e = VExpr (type_bool, e)
  let force_bool l v =
    match v with
    | VScalar v -> Either.Left (Value.to_bool l v)
    | VExpr (t,e) when t = type_bool -> Either.Right e
    | _ -> raise (SymbolicError (l, "boolean expected.  Got " ^ pp_value v))

  let not_bool l v = 
    match force_bool l v with
    | Left b -> vbool b
    | Right e -> ebool (apply "not_bool" [] [e])

  let and_bool l v1 v2 = 
    match force_bool l v1, force_bool l v2 with
    | (Left b1, Left b2) -> vbool (b1 && b2)
    | _ -> ebool (apply "and_bool" [] [value_expr l v1;value_expr l v2])

  let eq l v1 v2 = 
    match v1, v2 with
    | VScalar v1, VScalar v2 -> vbool (Value.eval_eq l v1 v2)
    | _ -> raise (SymbolicError (l, "todo"))

  (* Int *)
  let type_integer = Type_Constructor(Ident "integer")
  let vint i = VScalar (Value.VInt i)
  let eint e = VExpr (type_integer, e)
  let force_int l v =
    match v with
    | VScalar v -> Either.Left (Value.to_integer l v)
    | VExpr (t,e) when t = type_integer -> Either.Right e
    | _ -> raise (SymbolicError (l, "integer expected.  Got " ^ pp_value v))

  let add_int l v1 v2 = 
    match force_int l v1, force_int l v2 with
    | Left v1, Left v2 -> vint (prim_add_int v1 v2)
    | _ -> eint (apply "add_int" [] [value_expr l v1; value_expr l v2])

  let sub_int l v1 v2 = 
    match force_int l v1, force_int l v2 with
    | Left v1, Left v2 -> vint (prim_sub_int v1 v2)
    | _ -> eint (apply "sub_int" [] [value_expr l v1; value_expr l v2])

  let leq_int l v1 v2 = 
    match force_int l v1, force_int l v2 with
    | Left v1, Left v2 -> vbool (prim_le_int v1 v2)
    | _ -> ebool (apply "leq_int" [] [value_expr l v1; value_expr l v2])

  (* Bitvector *)
  let int_of_expr (e: expr): Z.t =
    match e with
    | Expr_LitInt(i) ->     (Z.of_string i)
    | Expr_LitHex(i) ->     (Z.of_string_base 16 (Value.drop_chars i '_'))
    | _ -> failwith @@ "int_of_expr: cannot coerce to int " ^ pp_expr e
  
  let concat (loc: AST.l) (xs: value list): value =
    raise (SymbolicError (loc, "TODO: support bitvector concat"))
  let extract_bits (loc: AST.l) = 
    raise (SymbolicError (loc, "TODO: support bitvector extract"))
  let width_bits loc x = 
    match x with
    | VScalar v -> vint (Z.of_int (Primops.prim_length_bits (Value.to_bits loc v)))
    | VExpr (Type_Bits(n),_) -> vint (int_of_expr n)
    | _ -> raise (Value.EvalError (loc, "bits expected. Got "^ pp_value x))
  let insert_bits loc = 
    raise (SymbolicError (loc, "TODO: support bitvector insert"))
  let inmask loc v1 v2 = 
    raise (SymbolicError (loc, "TODO: support bitvector inmask"))

  (* Records *)
  let get_field (loc: AST.l) (x: value) (f: AST.ident): value =
    match x with
    | VRecord fs -> Bindings.find f fs
    | VExpr _ -> raise (SymbolicError (loc, "TODO: support for expr field get. Got " ^ pp_value x))
    | _ -> raise (Value.EvalError (loc, "record expected. Got "^ pp_value x))

  let set_field (loc: AST.l) (x: value) (f: AST.ident) (v: value): value =
    match x with
    | VRecord fs -> VRecord (Bindings.add f v fs)
    | VExpr _ -> raise (SymbolicError (loc, "TODO: support for expr field set. Got " ^ pp_value x))
    | _ -> raise (Value.EvalError (loc, "record expected. Got "^ pp_value x))

  let new_record (fs: (AST.ident * value) list): value =
    VRecord (mk_bindings fs)

  (* Array *)
  let get_array (loc: AST.l) (a: value) (i: value): value =
    match (a, i) with
    | (VArray (x, d), VScalar (VInt  i')) -> prim_read_array x (Z.to_int i') d
    | (VArray (x, d), VScalar (VEnum i')) -> prim_read_array x (snd i') d
    | (VArray (x, d), VExpr _) -> 
        raise (SymbolicError (loc, "TODO: support for expr array index access. Got "^pp_value i))
    | (VArray (x, d), _) -> 
        raise (Value.EvalError (loc, "array index expected. Got "^pp_value i))
    | (VExpr _, _) ->
        raise (SymbolicError (loc, "TODO: support for expr array. Got "^pp_value i))
    | _ -> raise (Value.EvalError (loc, "array expected. Got "^pp_value a))

  let set_array (loc: AST.l) (a: value) (i: value) (v: value): value =
    match (a, i) with
    | (VArray (x, d), VScalar (VInt  i')) -> VArray (prim_write_array x (Z.to_int i') v, d)
    | (VArray (x, d), VScalar (VEnum i')) -> VArray (prim_write_array x (snd i') v, d)
    | (VArray (x, d), VExpr _) -> 
        raise (SymbolicError (loc, "TODO: support for expr array index access. Got "^pp_value i))
    | (VArray (x, d), _) -> 
        raise (Value.EvalError (loc, "array index expected. Got "^pp_value i))
    | (VExpr _, _) ->
        raise (SymbolicError (loc, "TODO: support for expr array. Got "^pp_value i))
    | _ -> raise (Value.EvalError (loc, "array expected. Got "^ pp_value a))

  let new_array (d: value): value =
    VArray (prim_empty_array, d)

  (* Unknown *)
  let to_integer l x = Value.to_integer l (to_scalar l x)

  let unknown_integer _ = VScalar (Value.eval_unknown_integer ())
  let unknown_string _  = VScalar (Value.eval_unknown_string ())
  let unknown_real _    = VScalar (Value.eval_unknown_real ())
  let unknown_bits l v  = VScalar (Value.eval_unknown_bits (to_integer l v))
  let unknown_ram l v   = VScalar (Value.eval_unknown_ram (to_integer l v))
  let unknown_enum l es = match es with e::es -> e | _ -> VScalar (Value.VUninitialized)

end
