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
and eint = {sign: bool; min: vint; max: vint; width: vint; value: expr}
and vint = (bitvector, eint) Either.t
and expr =
  | ECall of (AST.ident * value list * value list)
  | EVar of AST.ident
  | EAccess of (expr * access)
  | EUnknown
and value =
  | VBool   of bool sym
  | VInt    of vint
  | VReal   of real sym
  | VMask   of mask sym
  | VString of string sym
  | VRAM    of ram sym
  | VExc    of (AST.l * exc)
  | VTuple  of (value list)
  | VRecord of (value Bindings.t)
  | VArray  of (value ImmutableArray.t * value)
and access =
  | CArray of vint
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
and pp_int (x: vint): string = 
  match x with
  | Left v -> prim_cvt_bits_str (Z.of_int v.n) v
  | Right n -> pp_expr n.value
and pp_value (x: value): string =
  match x with
  | VInt  n -> pp_int n
  | VBool (Left n)    -> prim_cvt_bool_str n
  | VReal (Left n)    -> prim_cvt_real_str n
  | VString (Left n)  -> "\"" ^ n ^ "\""
  | VBool (Right n)   -> pp_expr n
  | VReal (Right n)   -> pp_expr n
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
and lift_int (loc: AST.l) (i: vint): AST.expr =
  match i with
  | Left i -> AST.Expr_LitInt(Z.to_string i) (*AST.Expr_LitBits(Z.format ("%0" ^"b") i)*)
  | Right i -> lift_expr loc i.e
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

let mk_bigint (x: Z.t) =
  Either.Left x

let mk_int (x: int) =
  mk_bigint (Z.of_int x)

(* Modify the base expr of a value, collecting the access chain required to reach it *)
let rec map_base_expr (f: access list -> expr -> expr) (r: access list) (v: value): value =
  match v with
  | VBool (Right e)         -> VBool (Right (f r e))
  | VInt  (Right e)         -> VInt  (Right ({s=e.s; min=e.min; max=e.max; w=e.w; e=f r e.e}))
  | VReal (Right e)         -> VReal (Right (f r e))
  | VBits (Right {n=n;v=e}) -> VBits (Right {n=n; v=f r e})
  | VString (Right e)       -> VString (Right (f r e))
  | VRAM (Right e)          -> VRAM (Right (f r e))
  | VTuple vs               -> VTuple (List.mapi (fun k -> map_base_expr f (CTuple k::r)) vs)
  | VRecord bs              -> VRecord (Bindings.mapi (fun k -> map_base_expr f (CField k::r)) bs)
  | VArray (a,d)            -> VArray (ImmutableArray.mapi (fun k -> 
      map_base_expr f (CArray (mk_int k)::r)) a, map_base_expr f r d)
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

let to_int (loc: AST.l) (x: value): vint =
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

let zero: vint = Left (Z.zero)

let int_max (x: vint): Z.t =
  match x with
  | Left x -> x
  | Right n -> match n.max with Some v -> v | _ -> unsupported Unknown "huh"

let int_min (x: vint): Z.t =
  match x with
  | Left x -> x
  | Right n -> match n.min with Some v -> v | _ -> unsupported Unknown "huh"

let int_sign (x: vint): bool =
  match x with
  | Left x -> Z.lt x Z.zero
  | Right n -> n.s

let find_bitrep (sign: bool) (max: Z.t) (min: Z.t) =
  assert (Z.geq max min);
  if not sign then begin
    assert (Z.geq min Z.zero);
    ( Z.of_int (Z.log2up (Z.succ max)))
  end else begin
    let neg = if Z.leq min Z.zero then Z.of_int ((Z.log2up (Z.neg min)) + 1) else Z.zero in
    let pos = if Z.leq Z.zero max then Z.of_int ((Z.log2up (Z.succ max)) + 1) else Z.zero in
    ( Z.max neg pos)
  end

let ecall f te e = ECall (FIdent (f, 0), te, e)

let cast_int (x: vint) (s: bool) (w: Z.t): vint =
  let n = VInt (mk_bigint w) in
  match x with
  | Left x -> Left x
  | Right r -> 
      if w <= r.w then Right r else
      let f = if r.s then "SignExtend" else "ZeroExtend" in
      Right {s; min=r.min; max=r.max; w; e=ECall (FIdent (f, 0), [VInt (mk_bigint r.w); n], [VInt x; n])}

let sym_add_int (x: vint) (y: vint): vint =
  match x, y with
  | Left x', Left y' -> mk_bigint (prim_add_int x' y')
  | _ -> 
      let max = Z.max (Z.max (int_max x) (int_max y)) (Z.add (int_max x) (int_max y)) in
      let min = Z.min (Z.min (int_min x) (int_min y)) (Z.add (int_min x) (int_min y)) in
      let s = Z.lt min Z.zero || int_sign x || int_sign y in
      let w = find_bitrep s max min in
      Right {s; min=Some(min); max=Some(max); w; e=ecall "add_bits" [VInt (mk_bigint w)] [VInt (cast_int x s w); VInt (cast_int y s w)]}

let sym_sub_int (x: vint) (y: vint): vint =
  match x, y with
  | Left x', Left y' -> mk_bigint (prim_sub_int x' y')
  | _ -> 
      let max = Z.max (Z.max (int_max x) (int_max y)) (Z.sub (int_max x) (int_min y)) in
      let min = Z.min (Z.min (int_min x) (int_min y)) (Z.sub (int_min x) (int_max y)) in
      let s = Z.lt min Z.zero || int_sign x || int_sign y in
      let w = find_bitrep s max min in
      Right {s; min=Some(min); max=Some(max); w; e=ecall "sub_bits" [VInt (mk_bigint w)] [VInt (cast_int x s w); VInt (cast_int y s w)]}

(*
let sym_mul_int (x: Z.t sym) (y: Z.t sym): Z.t sym =
  match x, y with
  | Left x', Left y' -> Left (Z.mul x' y')
  | _, Left x
  | Left x, _ when Z.equal x Z.zero -> Left Z.zero
  | x, Left n
  | Left n, x when Z.equal n Z.one -> x
  | _ -> call "mul_int" [] [VInt x; VInt y]
*)

let sym_le_int (x: vint) (y: vint): bool sym =
  match x, y with
  | Left x', Left y' -> Left (Z.leq x' y')
  | _ -> 
      let max = Z.max (int_max x) (int_max y) in
      let min = Z.min (int_min x) (int_min y) in
      let s = Z.lt min Z.zero || int_sign x || int_sign y in
      let w = find_bitrep s max min in
      call "le_bits" [VInt (mk_bigint w)] [VInt (cast_int x s w); VInt (cast_int y s w)]

let sym_eq_int (x: vint) (y: vint): bool sym =
  match x, y with
  | Left x', Left y' -> Left (Z.equal x' y')
  | _ -> 
      let max = Z.max (int_max x) (int_max y) in
      let min = Z.min (int_min x) (int_min y) in
      let s = Z.lt min Z.zero || int_sign x || int_sign y in
      let w = find_bitrep s max min in
      call "eq_bits" [VInt (mk_bigint w)] [VInt (cast_int x s w); VInt (cast_int y s w)]

(* Bitvector *)

let sym_width_bits (x: vbits): vint =
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

let rec sym_extract_bits (x: vbits) (lo: vint) (wd: vint): vbits =
  match x, lo, wd with
  | _, _, Left i when i <= Z.zero ->
      Left empty_bits
  | Left x', Left lo', Left wd' ->
      Left (prim_extract x' lo' wd')
  | Right {n=_; v=ECall (FIdent ("extract_bits", 0), _, [VBits x'; VInt lo'; VInt wd'])}, _, _ ->
      sym_extract_bits x' (sym_add_int lo' lo) wd
  | Right {n=_; v=ECall (FIdent ("extract_bits", 0), _, [VInt x'; VInt lo'; VInt wd'])}, _, _ ->
      vcall wd "extract_bits" [] [VInt x'; VInt (sym_add_int lo' lo); VInt wd]
  | Right {n=_; v=ECall (FIdent ("append_bits", 0), _, [VBits x1; VBits x2])}, _, _ ->
      let t2 = sym_width_bits x2 in
      if sym_le_int t2 lo = Left true then
        sym_extract_bits x1 (sym_sub_int lo t2) wd
      else if sym_le_int (sym_add_int lo wd) t2 = Left true then
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

let sym_insert_bits (x: vbits) (lo: vint) (wd: vint) (y: vbits): vbits =
  let yw = match sym_width_bits y with Left v -> Either.Left v | _ -> wd in
  match x, lo, yw, y with
  | Left x', Left lo', Left yw', Left y' -> Left (prim_insert x' lo' yw' y')
  | _ ->
      let xw = sym_width_bits x in
      let up = sym_add_int lo yw in
      sym_concat_bits [sym_extract_bits x up (sym_sub_int xw up); y; sym_extract_bits x zero lo]

let int_to_bits (x: vint) (w: vint): vbits =
  match x, w with
  | Left x, Left w  -> Left {n=Z.to_int w; v=x}
  | Left x, Right w -> failwith ""
  | Right r, _      -> Right {n=Left (r.w); v=r.e}

let sym_int_to_bits (x: vint) (lo: vint) (wd: vint): vbits =
  sym_extract_bits (int_to_bits x (sym_add_int lo wd)) lo wd

  (*
  match x, lo, wd with
  | Left x', Left lo', Left wd' -> Left (prim_extract_int x' lo' wd')
  | _ -> vcall wd "extract_bits" [] [VInt x; VInt lo; VInt wd]
  *)

let sym_cvt_int_bits (x: vint) (y: vint): vbits =
  sym_extract_bits (int_to_bits x y) (mk_int 0) y

let sym_cvt_bits_uint (x: vbits): vint =
  match x with
  | Left x -> Left (prim_cvt_bits_uint x)
  | Right e -> 
      match sym_width_bits x with
      | Left w ->
          let max = Some (Z.pred (Z.shift_left Z.one (Z.to_int w))) in
          Right {s=false; min=Some (Z.zero); max; w; e=e.v}
      | Right _ -> unsupported Unknown "cast of bitvector with unknown length to integer"

let sym_cvt_bits_sint (x: vbits): vint =
  match x with
  | Left x -> Left (prim_cvt_bits_sint x)
  | Right e -> 
      match sym_width_bits x with
      | Left w ->
          let w' = Z.to_int w - 1 in
          let max = Some (Z.pred (Z.shift_left Z.one w')) in
          let min = Some (Z.neg (Z.shift_left Z.one w')) in
          Right {s=true; min; max; w; e=e.v}
      | Right _ -> unsupported Unknown "cast of bitvector with unknown length to integer"


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
  | VInt  _         -> VInt (Right {s=true; min=None; max=None; w=Z.zero; e=e})
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
