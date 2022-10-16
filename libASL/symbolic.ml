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
  | EAccess of (expr * access)
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
and access =
  | CArray of (Z.t sym)
  | CTuple of (int)
  | CField of (AST.ident)

(* Pretty Printing *)

let rec pp_args (x: value list): string = String.concat ", " (List.map pp_value x)
and pp_expr (x: expr): string =
  match x with
  | ECall (f,tes,es) -> Printf.sprintf "%s {%s} (%s)" (AST.pprint_ident f) (pp_args tes) (pp_args tes)
  | EVar (i) -> AST.pprint_ident i
  | EAccess (e,CArray i) -> pp_expr e ^ "[" ^ pp_int i ^ "]"
  | EAccess (e,CTuple i) -> pp_expr e ^ "(" ^ string_of_int i ^ ")"
  | EAccess (e,CField f) -> pp_expr e ^ "." ^ AST.pprint_ident f
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
  | VBool (Right n)   -> lift_expr loc  n
  | VReal (Right n)   -> lift_expr loc  n
  | VBits (Right n)   -> lift_expr loc  n.v
  | VString (Right n) -> lift_expr loc  n
  | _ -> unsupported loc @@ "casting unhandled value type to expression: " ^ pp_value v
and lift_int (loc: AST.l) (i: Z.t sym): AST.expr =
  match i with
  | Left i -> AST.Expr_LitInt(Z.to_string i)
  | Right i -> lift_expr loc  i
and lift_expr (loc: AST.l) (e: expr): AST.expr =
  match e with
  | ECall (FIdent("extract_bits",0), _, [v; lo; wd]) ->
      AST.Expr_Slices(to_expr loc v, [AST.Slice_LoWd(to_expr loc lo, to_expr loc wd)])
  | ECall (f,tes,es) -> AST.Expr_TApply (f, List.map (to_expr loc) tes, List.map (to_expr loc) es)
  | EVar v -> AST.Expr_Var v
  | EAccess (e,CArray i) -> AST.Expr_Array (lift_expr loc e,lift_int loc i)
  | EAccess (e,CTuple _) -> unsupported loc @@ "need temporaries to extract from a tuple"
  | EAccess (e,CField f) -> AST.Expr_Field (lift_expr loc e,f)
  | EUnknown -> unsupported loc @@ "unknown expression"

(* Conversion from basic values *)

let rec from_concrete (loc: AST.l) (v: Value.value) : value =
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
  | Value.VTuple ts      -> VTuple  (List.map (from_concrete loc) ts)
  | Value.VRecord fs     -> VRecord (Bindings.map (from_concrete loc) fs)
  | Value.VArray (a,v)   -> VArray  (ImmutableArray.map (from_concrete loc) a, from_concrete loc v)
  | Value.VUninitialized -> unsupported loc "from_concrete: insufficient information to build symbolic"

let rec to_concrete (loc: AST.l) (v: value): Value.value =
  match v with
  | VBool (Left x)   -> Value.VBool x
  | VInt (Left x)    -> Value.VInt x
  | VReal (Left x)   -> Value.VReal x
  | VBits (Left x)   -> Value.VBits x
  | VMask (Left x)   -> Value.VMask x
  | VString (Left x) -> Value.VString x
  | VRAM (Left x)    -> Value.VRAM x
  | VExc x           -> Value.VExc x
  | VTuple (vs)      -> Value.VTuple (List.map (to_concrete loc) vs)
  | VRecord (vs)     -> Value.VRecord (Bindings.map (to_concrete loc) vs)
  | VArray (vs, d)   -> Value.VArray (ImmutableArray.map (to_concrete loc) vs, to_concrete loc d)
  | _                -> unsupported Unknown "to_concrete: cannot coerce expression value to concrete value"

let rec map_expr (f: expr -> expr) (v: value): value =
  match v with
  | VBool (Right e)         -> VBool (Right (f e))
  | VInt  (Right e)         -> VInt  (Right (f e))
  | VReal (Right e)         -> VReal (Right (f e))
  | VBits (Right {n=n;v=e}) -> VBits (Right {n=n; v=f e})
  | VString (Right e)       -> VString (Right (f e))
  | VRAM (Right e)          -> VRAM (Right (f e))
  | VTuple vs               -> VTuple (List.map (map_expr f) vs)
  | VRecord bs              -> VRecord (Bindings.map (map_expr f) bs)
  | VArray (a,d)            -> VArray (ImmutableArray.map (map_expr f) a, map_expr f d)
  | _                       -> v

let subst_expr (e: expr) (v: value): value =
  map_expr (fun _ -> e) v

(* Modify the base expr of a value, collecting the access chain required to reach it *)
let rec map_base_expr (f: access list -> expr -> expr) (r: access list) (v: value): value =
  match v with
  | VBool (Right e)         -> VBool (Right (f r e))
  | VInt  (Right e)         -> VInt  (Right (f r e))
  | VReal (Right e)         -> VReal (Right (f r e))
  | VBits (Right {n=n;v=e}) -> VBits (Right {n=n; v=f r e})
  | VString (Right e)       -> VString (Right (f r e))
  | VRAM (Right e)          -> VRAM (Right (f r e))
  | VTuple vs               -> VTuple (List.mapi (fun k -> map_base_expr f (CTuple k::r)) vs)
  | VRecord bs              -> VRecord (Bindings.mapi (fun k -> map_base_expr f (CField k::r)) bs)
  | VArray (a,d)            -> VArray (ImmutableArray.mapi (fun k -> 
      map_base_expr f (CArray (Left (Z.of_int k))::r)) a, map_base_expr f r d)
  | _                       -> v

(** Build a chained access expression, given a list order by outer most access first *)
let rec chained_access (r: access list) (e: expr): expr =
  match r with
  | x::xs -> chained_access xs (EAccess(e,x))
  | _ -> e

let subst_base (e: expr) (v: value): value =
  map_base_expr (fun l _ -> chained_access l e) [] v

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

let vfalse = VBool (Left false)
let vtrue = VBool (Left true)

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

let sym_mul_int (x: Z.t sym) (y: Z.t sym): Z.t sym =
  match x, y with
  | Left x', Left y' -> Left (Z.mul x' y')
  | _, Left x
  | Left x, _ when Z.equal x Z.zero -> Left Z.zero
  | x, Left n
  | Left n, x when Z.equal n Z.one -> x
  | _ -> call "mul_int" [] [VInt x; VInt y]

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
  | Left z, y
  | y, Left z when z.v = Z.zero -> Left z
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
      if sym_eq_int (sym_width_bits x) wd = Left true then x
      else vcall wd "extract_bits" [] [VBits x; VInt lo; VInt wd]

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

let rec to_type (loc: AST.l) (v: value): AST.ty =
  match v with
  | VBool _        -> Type_Constructor (Ident "boolean")
  | VInt _         -> Type_Constructor (Ident "integer")
  | VReal _        -> Type_Constructor (Ident "real")
  | VBits v        -> Type_Bits (lift_int loc (sym_width_bits v))
  | VMask _        -> Type_Constructor (Ident "__mask")
  | VString _      -> Type_Constructor (Ident "string")
  | VRAM _         -> Type_Constructor (Ident "__RAM")
  | VExc _         -> TC.type_exn
  | VTuple (vs)    -> Type_Tuple (List.map (to_type loc) vs)
  | VArray (vs, d) -> Type_Array (Index_Enum (Ident "dummy"), to_type loc d)
  | VRecord _      -> Type_Constructor (Ident "unknown")

let copy_scalar_type (e: expr) (v: value): value =
  match v with
  | VBool _         -> VBool (Right e)
  | VInt  _         -> VInt  (Right e)
  | VReal _         -> VReal (Right e)
  | VBits v         -> VBits (Right {n=sym_width_bits v; v=e})
  | VString _       -> VString (Right e)
  | VRAM _          -> VRAM (Right e)
  | _               -> invalid_arg @@ "not a scalar type: " ^ pp_value v

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

let sym_new_array (d: value): value =
  VArray (prim_empty_array, d)

(** Return a value given the index is defined and held within the array,
    otherwise builds an access expression based on the array's default value *)
let sym_get_array (loc: AST.l) (a: value) (i: value): value =
  match a, i with
  | VArray (x, d), VInt e ->
      (let de = map_base_expr (fun r b -> chained_access (CArray e::r) b) [] d in
      match e with
      | Left i -> prim_read_array x (Z.to_int i) de
      | _ -> de)
  | VArray (x, d), _ -> symerror loc @@ "array index expected. Got " ^ pp_value i
  | _ -> symerror loc @@ "array expected. Got " ^ pp_value a

(** In the event of a symbolic index update, assumes the array's default value d is 
    a sound over-approximation of the introduced value v and the array's existing entries *)
let sym_set_array (loc: AST.l) (a: value) (i: value) (v: value): value =
  match a, i with
  | VArray (x, d), VInt (Left i') -> VArray (prim_write_array x (Z.to_int i') v, d)
  | VArray (x, d), VInt (Right e) -> sym_new_array d
  | VArray (x, d), _ -> symerror loc @@ "array index expected. Got " ^ pp_value i
  | _ -> symerror loc @@ "array expected. Got " ^ pp_value a

let record_merge (f: AST.ident -> 'a -> 'b -> 'c option) (l: 'a Bindings.t) (r: 'b Bindings.t): 'c Bindings.t =
  Bindings.merge (fun k v1 v2 ->
    match v1, v2 with
    | Some v1, Some v2 -> f k v1 v2
    | _ -> invalid_arg "merge of different record structures") l r

let array_default (d: value) (i: int) =
  map_base_expr (fun r b -> chained_access (CArray (Left (Z.of_int i))::r) b) [] d

let array_merge (f: int -> value -> value -> 'a Option.t) (la: value ImmutableArray.t) (ld: value) (ra: value ImmutableArray.t) (rd: value): 'a ImmutableArray.t =
  ImmutableArray.merge (fun k v1 v2 ->
    match v1, v2 with
    | Some v1, Some v2 -> f k v1 v2
    | Some v1, _ -> f k v1 (array_default rd k)
    | _, Some v2 -> f k (array_default ld k) v2
    | _ -> None) la ra
