(****************************************************************
 * ASL symbolic interpreter values
 ****************************************************************)

module AST = Asl_ast
module TC = Tcheck

open Primops
open Asl_utils

exception SymbolicError of (AST.l * string)
exception UnsupportedError of (AST.l * string)
let symerror loc err = raise (SymbolicError (loc, err))
let unsupported loc err = raise (UnsupportedError (loc, err))

type 'a sym = ('a, expr) Either.t
and sbitvector = {n: Z.t sym; v: expr}
and vbits = (bitvector, sbitvector) Either.t
and expr =
  | ECall of (AST.ident * value list * value list)
  | EVar of AST.ident
  | EUnknown
and value =
  | VBool   of bool sym
  | VInt    of Z.t sym
  | VReal   of real sym
  | VBits   of vbits
  | VMask   of mask sym
  | VString of string sym
  | VRAM    of ram sym

  | VExc    of (AST.l * exc)
  | VTuple  of (value list)
  | VRecord of (value Bindings.t)
  | VArray  of (value ImmutableArray.t * value)

(* Pretty Printing *)

let rec pp_args (x: value list): string = String.concat ", " (List.map pp_value x)
and pp_expr (x: expr): string =
  match x with
  | ECall (f,tes,es) -> Printf.sprintf "%s {%s} (%s)" (AST.pprint_ident f) (pp_args tes) (pp_args tes)
  | EVar (i) -> AST.pprint_ident i
  | EUnknown -> "UNINITIALIZED"
and pp_int (x: Z.t sym): string = Either.fold ~left:prim_cvt_int_decstr ~right:pp_expr x
and pp_value (x: value): string =
  match x with
  | VInt  n -> pp_int n
  | VBool (Left n)    -> prim_cvt_bool_str n
  | VReal (Left n)    -> prim_cvt_real_str n
  | VBits (Left n)    -> prim_cvt_bits_str (Z.of_int n.n) n
  | VString (Left n)  -> "\"" ^ n ^ "\""
  | VBool (Right n)   -> pp_expr n
  | VReal (Right n)   -> pp_expr n
  | VBits (Right n)   -> pp_expr n.v
  | VString (Right n) -> pp_expr n
  | VMask   m         -> "todo: mask"
  | VExc (loc, exc)   ->
      let msg = (match exc with
      | Exc_ConstrainedUnpredictable -> "ConstrainedUnpredictable"
      | Exc_ExceptionTaken           -> "ExceptionTaken"
      | Exc_ImpDefined s             -> "ImpDefined" ^ s
      | Exc_SEE s                    -> "SEE" ^ s
      | Exc_Undefined                -> "Undefined"
      | Exc_Unpredictable            -> "Unpredictable"
      ) in
      "Exception " ^ msg ^ " at " ^ Asl_ast.pp_loc loc
  | VTuple  vs -> "(" ^ pp_args vs ^ ")"
  | VRecord fs ->
      let fs' = List.map (fun (f, v) -> "."^ AST.pprint_ident f ^" = "^ pp_value v) (Bindings.bindings fs) in
      "{" ^ String.concat ", " fs' ^ "}"
  | VArray (a, _) ->
      let vs = List.map (fun (i, v) -> string_of_int i ^":"^ pp_value v) (ImmutableArray.bindings a) in
      "[" ^ String.concat ", " vs ^ "]"
  | VRAM _ -> "RAM"

(* Conversion to expr *)

let rec to_expr (loc: AST.l) (v: value): AST.expr =
  match v with
  | VBool (Left n)    -> AST.Expr_Var(if n then Ident "TRUE" else Ident "FALSE")
  | VReal (Left n)    -> AST.Expr_LitReal(Q.to_string n)
  | VBits (Left n)    -> AST.Expr_LitBits(Z.format ("%0" ^ string_of_int n.n ^ "b") n.v)
  | VString (Left n)  -> AST.Expr_LitString n
  | VInt  n           -> lift_int  loc n
  | VBool (Right n)   -> lift_expr loc TC.type_bool n
  | VReal (Right n)   -> lift_expr loc TC.type_real n
  | VBits (Right n)   -> lift_expr loc (Type_Bits (lift_int loc n.n)) n.v
  | VString (Right n) -> lift_expr loc TC.type_string n
  | _ -> unsupported loc @@ "casting unhandled value type to expression: " ^ pp_value v
and lift_int (loc: AST.l) (i: Z.t sym): AST.expr =
  match i with
  | Left i -> AST.Expr_LitInt(Z.to_string i)
  | Right i -> lift_expr loc TC.type_integer i
and lift_expr (loc: AST.l) (t: AST.ty) (e: expr): AST.expr =
  match e with
  | ECall (FIdent("extract_bits",0), _, [v; lo; wd]) ->
      AST.Expr_Slices(to_expr loc v, [AST.Slice_LoWd(to_expr loc lo, to_expr loc wd)])
  | ECall (f,tes,es) -> AST.Expr_TApply (f, List.map (to_expr loc) tes, List.map (to_expr loc) es)
  | EVar v -> AST.Expr_Var v
  | EUnknown -> AST.Expr_Unknown t

(* Conversion from basic values *)

let rec from_value (v: Value.value) : value =
  match v with
  | Value.VBool i        -> VBool (Left i)
  | Value.VEnum (_,i)    -> VInt (Left (Z.of_int i))
  | Value.VInt i         -> VInt (Left i)
  | Value.VReal i        -> VReal (Left i)
  | Value.VBits i        -> VBits (Left i)
  | Value.VMask i        -> VMask (Left i)
  | Value.VString i      -> VString (Left i)
  | Value.VRAM a         -> VRAM (Left a)
  | Value.VExc e         -> VExc e
  | Value.VTuple ts      -> VTuple (List.map from_value ts)
  | Value.VRecord fs     -> VRecord (Bindings.map from_value fs)
  | Value.VArray (a,v)   -> VArray (ImmutableArray.map from_value a, from_value v)
  | Value.VUninitialized ->
      unsupported Unknown "uninitialized value has insufficient information to build symbolic"

(* Destructors *)

let to_bool (loc: AST.l) (x: value): bool sym =
  match x with
  | VBool b -> b
  | _ -> symerror loc @@ "boolean expected.  Got " ^ pp_value x

let to_int (loc: AST.l) (x: value): Z.t sym =
  match x with
  | VInt b -> b
  | _ -> symerror loc @@ "integer expected.  Got " ^ pp_value x

let to_mask (loc: AST.l) (x: value): mask sym =
  match x with
  | VMask m -> m
  | _ -> symerror loc @@ "mask expected.  Got " ^ pp_value x

let to_bits (loc: AST.l) (x: value): vbits =
  match x with
  | VBits v -> v
  | _ -> symerror loc @@ "bitvector expected. Got " ^ pp_value x

let call f tes es = Either.Right (ECall (FIdent (f,0), tes, es))
let vcall w f tes es = Either.Right {n=w; v = ECall (FIdent (f,0), tes, es)}

(* Boolean *)

let sym_not_bool (x: bool sym): bool sym =
  match x with
  | Left b -> Left (not b)
  | _ -> call "not_bool" [] [VBool x]

let sym_and_bool (x: bool sym) (y: bool sym): bool sym =
  match x, y with
  | Left x', Left y' -> Left (x' && y')
  | Left false, _
  | _, Left false -> Left false
  | _ -> call "and_bool" [] [VBool x; VBool y]

let sym_eq_bool (x: bool sym) (y: bool sym): bool sym =
  match x, y with
  | Left x', Left y' -> Left (x' = y')
  | _ -> call "eq_bool" [] [VBool x; VBool y]

(* Integer *)

let zero: Z.t sym = Left (Z.zero)

let sym_add_int (x: Z.t sym) (y: Z.t sym): Z.t sym =
  match x, y with
  | Left x', Left y' -> Left (Z.add x' y')
  | Left x, y
  | y, Left x when x = Z.zero -> y
  | _ -> call "add_int" [] [VInt x; VInt y]

let sym_sub_int (x: Z.t sym) (y: Z.t sym): Z.t sym =
  match x, y with
  | Left x', Left y' -> Left (Z.sub x' y')
  | y, Left x when x = Z.zero -> y
  | _ -> call "sub_int" [] [VInt x; VInt y]

let sym_leq_int (x: Z.t sym) (y: Z.t sym): bool sym =
  match x, y with
  | Left x', Left y' -> Left (Z.leq x' y')
  | _ -> call "leq_int" [] [VInt x; VInt y]

let sym_eq_int (x: Z.t sym) (y: Z.t sym): bool sym =
  match x, y with
  | Left x', Left y' -> Left (Z.equal x' y')
  | _ -> call "eq_int" [] [VInt x; VInt y]

(* Bitvector *)

let sym_width_bits (x: vbits): Z.t sym =
  match x with
  | Left x' -> Left (Z.of_int x'.n)
  | Right x' -> x'.n

let sym_eq_bits (x: vbits) (y: vbits): bool sym =
  match x, y with
  | Left x', Left y' -> Left (prim_eq_bits x' y')
  | _ -> call "eq_bits" [VInt (sym_width_bits x)] [VBits x; VBits y]

let sym_and_bits (x: vbits) (y: vbits): vbits =
  let w = sym_width_bits x in
  match x, y with
  | Left x', Left y' -> Left (prim_and_bits x' y')
  | _ -> vcall w "and_bits" [VInt w] [VBits x; VBits y]

let sym_in_mask (x: vbits) (m: mask sym): bool sym =
  let w = sym_width_bits x in
  match m with
  | Left m' ->
      let f: vbits = Left {n=m'.n; v=m'.m} in
      let v: vbits = Left {n=m'.n; v=m'.v} in
      sym_eq_bits (sym_and_bits x f) v
  | _ -> call "in_mask" [VInt w] [VBits x; VMask m]

let sym_append_bits (x: vbits) (y: vbits): vbits =
  let xw = sym_width_bits x in
  let yw = sym_width_bits y in
  let w = sym_add_int xw yw in
  if xw = Left Z.zero then y else if yw = Left Z.zero then x
  else match x, y with
  | Left x', Left y' -> Left (prim_append_bits x' y')
  (* special case: if y is already an append operation,  fuse x into its y's left argument. *)
  | Left x', Right {n=_ ; v=ECall (FIdent ("append_bits", 0), [_; rw], [VBits (Left l);r])} ->
      let l' = Either.Left (prim_append_bits x' l) in
      let lw' = sym_width_bits (Left l) in
      vcall w "append_bits" [VInt (sym_add_int xw lw'); rw] [VBits l'; r]
  | _ ->
      vcall w "append_bits" [VInt xw; VInt yw] [VBits x; VBits y]

let rec sym_extract_bits (x: vbits) (lo: Z.t sym) (wd: Z.t sym): vbits =
  match x, lo, wd with
  | _, _, Left i when i <= Z.zero ->
      Left empty_bits
  | Left x', Left lo', Left wd' ->
      Left (prim_extract x' lo' wd')
  | Right {n=_; v=ECall (FIdent ("extract_bits", 0), _, [VBits x'; VInt lo'; VInt wd'])}, _, _ ->
      sym_extract_bits x' (sym_add_int lo' lo) wd
  | Right {n=_; v=ECall (FIdent ("append_bits", 0), _, [VBits x1; VBits x2])}, _, _ ->
      let t2 = sym_width_bits x2 in
      if sym_leq_int t2 lo = Left true then
        sym_extract_bits x1 (sym_sub_int lo t2) wd
      else if sym_leq_int (sym_add_int lo wd) t2 = Left true then
        sym_extract_bits x2 lo wd
      else
        let w2 = sym_sub_int t2 lo in
        let w1 = sym_sub_int wd w2 in
        sym_append_bits (sym_extract_bits x1 zero w1) (sym_extract_bits x2 lo w2)
  | _ ->
      vcall wd "extract_bits" [] [VBits x; VInt lo; VInt wd]

let sym_concat_bits (xs: vbits list): vbits =
  match xs with
  | [] -> Left empty_bits
  | x::xs -> List.fold_left sym_append_bits x xs

let sym_insert_bits (x: vbits) (lo: Z.t sym) (wd: Z.t sym) (y: vbits): vbits =
  let yw = match sym_width_bits y with Left v -> Either.Left v | _ -> wd in
  match x, lo, yw, y with
  | Left x', Left lo', Left yw', Left y' -> Left (prim_insert x' lo' yw' y')
  | _ ->
      let xw = sym_width_bits x in
      let up = sym_add_int lo yw in
      sym_concat_bits [sym_extract_bits x up (sym_sub_int xw up); y; sym_extract_bits x zero lo]

let sym_int_to_bits (x: Z.t sym) (lo: Z.t sym) (wd: Z.t sym): vbits =
  match x, lo, wd with
  | Left x', Left lo', Left wd' -> Left (prim_extract_int x' lo' wd')
  | _ -> vcall wd "extract_bits" [] [VInt x; VInt lo; VInt wd]

(* Real *)

let sym_eq_real (x: real sym) (y: real sym): bool sym =
  match x, y with
  | Left x', Left y' -> Left (prim_eq_real x' y')
  | _ -> call "eq_real" [] [VReal x; VReal y]

(* String *)

let sym_eq_str (x: string sym) (y: string sym): bool sym =
  match x, y with
  | Left x', Left y' -> Left (prim_eq_str x' y')
  | _ -> call "eq_str" [] [VString x; VString y]

(* Generic *)

let rec sym_eq (loc: AST.l) (x: value) (y: value): bool sym =
  match x, y with
  | VBool   x', VBool   y' -> sym_eq_bool x' y'
  | VInt    x', VInt    y' -> sym_eq_int x' y'
  | VBits   x', VBits   y' -> sym_eq_bits x' y'
  | VReal   x', VReal   y' -> sym_eq_real x' y'
  | VString x', VString y' -> sym_eq_str x' y'
  | VTuple  x', VTuple  y' ->
      List.fold_left2 (fun b v1 v2 -> sym_and_bool b (sym_eq loc v1 v2)) (Left true) x' y'
  | _ -> symerror loc @@ "matchable scalar types expected. Got " ^ pp_value x ^ " " ^ pp_value y

(* Records *)

let sym_get_field (loc: AST.l) (x: value) (f: AST.ident): value =
  match x with
  | VRecord fs -> Bindings.find f fs
  | _ -> symerror loc @@ "record expected. Got " ^ pp_value x

let sym_set_field (loc: AST.l) (x: value) (f: AST.ident) (v: value): value =
  match x with
  | VRecord fs -> VRecord (Bindings.add f v fs)
  | _ -> symerror loc @@ "record expected. Got " ^ pp_value x

let sym_new_record (fs: (AST.ident * value) list): value =
  VRecord (mk_bindings fs)

(* Array *)

let sym_get_array (loc: AST.l) (a: value) (i: value): value =
  match a, i with
  | VArray (x, d), VInt (Left i') -> prim_read_array x (Z.to_int i') d
  | VArray (x, d), VInt (Right e) ->
      unsupported loc @@ "symbolic array index. Got " ^ pp_value a ^ "[" ^ pp_value i ^"]"
  | VArray (x, d), _ -> symerror loc @@ "array index expected. Got " ^ pp_value i
  | _ -> symerror loc @@ "array expected. Got " ^ pp_value a

let sym_set_array (loc: AST.l) (a: value) (i: value) (v: value): value =
  match a, i with
  | VArray (x, d), VInt (Left i') -> VArray (prim_write_array x (Z.to_int i') v, d)
  | VArray (x, d), VInt (Right e) ->
      unsupported loc @@ "symbolic array index. Got " ^ pp_value a ^ "[" ^ pp_value i ^"]:=" ^ pp_value v
  | VArray (x, d), _ -> symerror loc @@ "array index expected. Got " ^ pp_value i
  | _ -> symerror loc @@ "array expected. Got " ^ pp_value a

let sym_new_array (d: value): value =
  VArray (prim_empty_array, d)

module SymbolicValue : Abstract_interface.Value = struct
  type t = value

  (* Value Constructors *)
  let from_bool (x: bool): value = VBool (Left x)
  let from_int  (x: int) : value = VInt (Left (Z.of_int x))
  let from_enum x y      : value = VInt (Left (Z.of_int y))
  let from_exc x y       : value = VExc (x,y)
  let from_tuple l       : value = VTuple l

  (* Parsers *)
  let from_intLit    s = from_value (Value.from_intLit s)
  let from_hexLit    s = from_value (Value.from_hexLit s)
  let from_realLit   s = from_value (Value.from_realLit s)
  let from_bitsLit   s = from_value (Value.from_bitsLit s)
  let from_maskLit   s = from_value (Value.from_maskLit s)
  let from_stringLit s = from_value (Value.from_stringLit s)

  (* Value Destructors *)
  let to_tuple (loc: AST.l) (x: value): value list =
    match x with
    | VTuple xs -> xs
    | _ -> symerror loc @@ "tuple expected. Got " ^ pp_value x
  let to_string (loc: AST.l) (x: value): string =
    match x with
    | VString (Left v) -> v
    | VString (Right e) ->
        unsupported loc @@ "can't convert symbolic string to concrete value. Got " ^ pp_value x
    | _ -> symerror loc @@ "string expected. Got " ^ pp_value x
  let to_exc (loc: AST.l) (x: value): (AST.l * exc) =
    match x with
    | VExc v -> v
    | _ -> symerror loc @@ "exception expected. Got " ^ pp_value x

  (* Unit *)
  let vunit = VTuple []
  let is_unit v =
    match v with
    | VTuple [] -> true
    | _ -> false

  (* Boolean *)
  let not_bool (loc: AST.l) (x: value): value =
    VBool (sym_not_bool (to_bool loc x))
  let and_bool (loc: AST.l) (x: value) (y: value): value =
    VBool (sym_and_bool (to_bool loc x) (to_bool loc y))
  let eq (loc: AST.l) (x: value) (y: value): value =
    VBool (sym_eq loc x y)

  (* Integer *)
  let add_int (loc: AST.l) (x: value) (y: value): value =
    VInt (sym_add_int (to_int loc x) (to_int loc y))
  let sub_int (loc: AST.l) (x: value) (y: value): value =
    VInt (sym_sub_int (to_int loc x) (to_int loc y))
  let leq_int (loc: AST.l) (x: value) (y: value): value =
    VBool (sym_leq_int (to_int loc x) (to_int loc y))

  (* Bitvector *)
  let concat_bits (loc: AST.l) (x: value list) : value =
    VBits (sym_concat_bits (List.map (to_bits loc) x))
  let extract_bits (loc: AST.l) (x: value) (lo: value) (wd: value) : value =
    match x with
    | VInt v -> VBits (sym_int_to_bits v (to_int loc lo) (to_int loc wd))
    | _ -> VBits (sym_extract_bits (to_bits loc x) (to_int loc lo) (to_int loc wd))
  let width_bits (loc: AST.l) (x: value) : value =
    VInt (sym_width_bits (to_bits loc x))
  let insert_bits (loc: AST.l) (x: value) (lo: value) (wd: value) (y: value) : value =
    VBits (sym_insert_bits (to_bits loc x) (to_int loc lo) (to_int loc wd) (to_bits loc y))
  let in_mask (loc: AST.l) (x: value) (m: value): value =
    VBool (sym_in_mask (to_bits loc x) (to_mask loc m))

  (* Records *)
  let get_field = sym_get_field
  let set_field = sym_set_field
  let new_record = sym_new_record

  (* Array *)
  let get_array = sym_get_array
  let set_array = sym_set_array
  let new_array = sym_new_array

  (* Unknown *)
  let unknown_integer _   = VInt (Right EUnknown)
  let unknown_real    _   = VReal (Right EUnknown)
  let unknown_string  _   = VString (Right EUnknown)
  let unknown_bits    l w = VBits (Right {n=to_int l w; v=EUnknown})
  let unknown_enum    _ _ = VInt (Right EUnknown)
  let unknown_ram     _ _ = VRAM (Right EUnknown)

end
