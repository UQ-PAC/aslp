(****************************************************************
 * ASL disassembler
 ****************************************************************)

(** ASL disassembler *)

module PP   = Asl_parser_pp
module AST  = Asl_ast
module TC   = Tcheck

open AST
open Asl_utils
open Abstract_interface
open Symbolic

exception Throw of (AST.l * Primops.exc)

let int_expr (i: int): AST.expr = AST.Expr_LitInt (string_of_int i)

let bool_expr (b: bool): AST.expr = AST.Expr_Var(if b then Ident "TRUE" else Ident "FALSE")

let sym_pure_prim (f: string) (tvs: value list) (vs: value list): value option =
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

(****************************************************************
 * Flags to control behaviour (mostly for debugging)
 ****************************************************************)

(** Debugging output on every variable write *)
let trace_write = ref false

(** Debugging output on every function call *)
let trace_funcall = ref false

(** Debugging output on every primitive function or function call *)
let trace_primop = ref false

(** Debugging output on every instruction execution *)
let trace_instruction = ref false

(** It is an error to have multiple function definitions with conflicting types.
 *  But, for historical reasons, we still allow multiple definitions and later
 *  definitions override earlier definitions.
 *)
let override_conflicts = true


(****************************************************************)
(** {2 Lookup table for IMPLEMENTATION_DEFINED values}          *)
(****************************************************************)

module ImpDefs = struct
    include Map.Make(struct
        type t = string
        let compare = String.compare
    end)
end


(****************************************************************)
(** {2 Residual Program}                                        *)
(****************************************************************)

(* A residual program with early exit conditions *)
type 'a resid =
  | Open   of stmt list
  | Closed of (stmt list * 'a)
  | Branch of (expr * 'a resid * 'a resid)

(* Extend the residual program where it has not yet exited *)
let rec push (res: 'a resid) (ps: stmt list): 'a resid =
  match res with
  | Open s -> Open (s @ ps)
  | Branch (c,ls,rs) -> Branch (c,push ls ps,push rs ps)
  | _ -> res


(****************************************************************)
(** {2 Stashed Value}                                           *)
(****************************************************************)

(** A partially stashed value, with expansion for aggregates *)
type stashed = 
  | SVal of value
  | SIdent of ident * value
  | STuple of stashed list 
  | SRecord of stashed Bindings.t 
  | SArray of (stashed Primops.ImmutableArray.t * stashed)

(** Extract a value from the stashed representation *)
let rec value_of_stashed (s: stashed): value =
  match s with
  | SVal v -> v
  | SIdent (i,v) -> v
  | STuple ts -> VTuple (List.map value_of_stashed ts)
  | SRecord fs -> VRecord (Bindings.map value_of_stashed fs)
  | SArray (a,d) -> VArray (Primops.ImmutableArray.map value_of_stashed a, value_of_stashed d)

(** Build a unstashed value from some initial value *)
let rec stashed_of_value (v: value): stashed =
  match v with
  | VTuple ts -> STuple (List.map stashed_of_value ts)
  | VRecord fs -> SRecord (Bindings.map stashed_of_value fs)
  | VArray (a,d) -> SArray (Primops.ImmutableArray.map stashed_of_value a, stashed_of_value d)
  | v -> SVal v 

let force_merge (f: 'a -> 'b -> 'c) k (a: 'a option) (b: 'b option): 'c option =
  match a, b with
  | Some a', Some b' -> Some (f a' b')
  | _ -> invalid_arg "Maps don't have the same keys"

let array_merge (f: 'a -> 'b -> 'c) (dl: 'k -> 'a) (dr: 'k -> 'b) k (a: 'a option) (b: 'b option): 'c option =
  match a, b with
  | Some a', Some b' -> Some (f a' b')
  | Some a', None    -> Some (f a' (dr k))
  | None, Some b'    -> Some (f (dl k) b')
  | _ -> None

(** Create new stashed variables *)
let rec merge_stashed_value (f: value -> ident) (s: stashed) (v: value): stashed =
  let loop s v = merge_stashed_value f s v in
  match s, v with
  | STuple ts, VTuple vs -> STuple (List.map2 loop ts vs)
  | SRecord fs, VRecord vs -> SRecord (Bindings.merge (force_merge loop) fs vs)
  | SArray (a,ds), VArray (vs,v) -> 
      SArray (Primops.ImmutableArray.merge (array_merge loop (fun _ -> ds) (fun i -> v)) a vs, ds)
  | SIdent (i,v), _ -> SIdent (i,v)
  | SVal v', _ -> if v' = v then s else let i = f v' in SIdent (i, v)
  | _ -> invalid_arg "merge of different aggregate types"

(****************************************************************)
(** {2 Scopes}                                                  *)
(****************************************************************)

(** Basically just a mutable binding *)
type scope = { mutable bs : value Bindings.t; }

let empty_scope (_: unit): scope =
  let bs = Bindings.empty in
  { bs }

let mem_scope (k: ident) (s: scope): bool =
  Bindings.mem k s.bs

let get_scope (k: ident) (s: scope): value =
  Bindings.find k s.bs

let get_scope_val (k: ident) (s: scope): value =
  (* value_of_value *) (Bindings.find k s.bs)

let get_scope_opt (k: ident) (s: scope): value option =
  Bindings.find_opt k s.bs

let set_scope (k: ident) (v: value) (s: scope): unit =
    s.bs <- Bindings.add k v s.bs


(****************************************************************)
(** {2 Mutable bindings}                                        *)
(****************************************************************)

type prog = AST.stmt list

(** Environment representing both global and local state of the system *)
module Env : sig
    type t

    val empty               : unit -> t

    val nestTop             : (t -> 'a) -> (t -> 'a * prog)
    val nest                : (t -> 'a) -> (t -> 'a * prog)

    val addLocalVar         : AST.l -> t -> ident -> value -> unit
    val addLocalConst       : AST.l -> t -> ident -> value -> unit

    val addGlobalConst      : t -> ident -> value -> unit
    val getGlobalConst      : t -> ident -> value

    val addEnum             : t -> ident -> value list -> unit
    val getEnum             : t -> ident -> (value list) option

    val addRecord           : t -> ident -> (AST.ty * ident) list -> unit
    val getRecord           : t -> ident -> (AST.ty * ident) list option

    val addTypedef          : t -> ident -> AST.ty -> unit
    val getTypedef          : t -> ident -> AST.ty option

    val addGlobalVar        : t -> ident -> value -> unit

    val getFun              : AST.l -> t -> ident -> fun_sig
    val addFun              : AST.l -> t -> ident -> fun_sig -> unit

    val getInstruction      : AST.l -> t -> ident -> (encoding * (stmt list) option * bool * stmt list)
    val addInstruction      : AST.l -> t -> ident -> (encoding * (stmt list) option * bool * stmt list) -> unit

    val getDecoder          : t -> ident -> decode_case
    val addDecoder          : t -> ident -> decode_case -> unit

    val setImpdef           : t -> string -> value -> unit
    val getImpdef           : AST.l -> t -> string -> value

    val getVar              : AST.l -> t -> ident -> value
    val setVar              : AST.l -> t -> ident -> value -> unit



    val mergeVars           : AST.l -> t -> t -> t -> unit
    val mergeValue          : AST.l -> t -> t -> t -> value -> value -> value
    val push                : prog -> t -> unit
    val residual            : t -> prog
    val copy                : t -> t
    (*val stashResult         : AST.l -> value -> t -> unit *)

end = struct
    type t = {
        mutable instructions : (encoding * (stmt list) option * bool * stmt list) Bindings.t;
        mutable decoders     : decode_case Bindings.t;
        mutable functions    : fun_sig Bindings.t;
        mutable enums        : (value list) Bindings.t;
        mutable records      : ((AST.ty * ident) list) Bindings.t;
        mutable typedefs     : AST.ty Bindings.t;
        mutable impdefs      : value ImpDefs.t;

        mutable globals      : scope;
        mutable constants    : scope;
        mutable locals       : scope list;
        mutable residual     : value resid;
        mutable decls        : AST.stmt list;
        mutable returnSyms   : stashed option;
        numSymbols           : int ref;
    }

    let empty _ = {
        decoders     = Bindings.empty;
        instructions = Bindings.empty;
        functions    = Bindings.empty;
        enums        = Bindings.empty;
        records      = Bindings.empty;
        typedefs     = Bindings.empty;
        impdefs      = ImpDefs.empty;

        globals      = empty_scope ();
        constants    = empty_scope ();
        locals       = [empty_scope ()];
        residual     = Open [];
        decls        = [];
        returnSyms   = None;
        numSymbols   = ref 0;
    }

    let nestTop (k: t -> 'a) (parent: t): 'a * prog =
        let child = {
            decoders     = parent.decoders;
            instructions = parent.instructions;
            functions    = parent.functions;
            enums        = parent.enums;
            records      = parent.records;
            typedefs     = parent.typedefs;
            globals      = parent.globals;
            constants    = parent.constants;
            impdefs      = parent.impdefs;
            locals       = [empty_scope ()];  (* only change *)

            residual     = Open [];
            decls        = [];
            returnSyms   = None;
            numSymbols   = parent.numSymbols;
        } in
        let r = k child in
        (r, []) (* child.residual) *)

    let nest (k: t -> 'a) (parent: t): 'a * prog =
        let child = {
            decoders     = parent.decoders;
            instructions = parent.instructions;
            functions    = parent.functions;
            enums        = parent.enums;
            records      = parent.records;
            typedefs     = parent.typedefs;
            globals      = parent.globals;
            constants    = parent.constants;
            impdefs      = parent.impdefs;
            locals       = empty_scope () :: parent.locals;  (* only change *)

            residual = Open [];
            decls = [];
            returnSyms   = parent.returnSyms;
            numSymbols = parent.numSymbols;
        } in
        let r = k child in
        (r, []) (* child.residual) *)

    let copy (env: t): t =
        {
            decoders     = env.decoders;
            instructions = env.instructions;
            functions    = env.functions;
            enums        = env.enums;
            records      = env.records;
            typedefs     = env.typedefs;
            globals      = { bs = env.globals.bs };
            constants    = env.constants;
            impdefs      = env.impdefs;
            locals       = List.map (fun x -> { bs = x.bs }) env.locals;

            residual   = Open [];
            decls = [];
            returnSyms = env.returnSyms;
            numSymbols = env.numSymbols;
        }

    let addEnum (env: t) (x: ident) (vs: value list): unit =
        env.enums    <- Bindings.add x vs env.enums

    let getEnum (env: t) (x: ident): (value list) option =
        Bindings.find_opt x env.enums

    let addRecord (env: t) (x: ident) (fs: (AST.ty * ident) list): unit =
        env.records <- Bindings.add x fs env.records

    let getRecord (env: t) (x: ident): ((AST.ty * ident) list) option =
        Bindings.find_opt x env.records

    let addTypedef (env: t) (x: ident) (ty: AST.ty): unit =
        env.typedefs <- Bindings.add x ty env.typedefs

    let getTypedef (env: t) (x: ident): AST.ty option =
        Bindings.find_opt x env.typedefs

    let getFun (loc: l) (env: t) (x: ident): fun_sig =
        (match Bindings.find_opt x env.functions with
        | Some def -> def
        | None     -> raise (SymbolicError (loc, "getFun " ^ pprint_ident x))
        )

    let addFun (loc: l) (env: t) (x: ident) (def: fun_sig): unit =
        if false then Printf.printf "Adding function %s\n" (pprint_ident x);
        if Bindings.mem x env.functions then begin
            if true then begin
                () (* silently override *)
            end else if override_conflicts then begin
                (* backward compatibility mode: only report a stern warning *)
                Printf.printf "Stern warning: %s function %s conflicts with earlier definition - discarding earlier definition\n"
                    (pp_loc loc) (pprint_ident x);
            end else begin
                raise (TC.Ambiguous (loc, "function definition", pprint_ident x))
            end
        end;
        env.functions <- Bindings.add x def env.functions

    let getInstruction (loc: AST.l) (env: t) (x: ident): (encoding * (stmt list) option * bool * stmt list) =
        Bindings.find x env.instructions

    let addInstruction (loc: AST.l) (env: t) (x: ident) (instr: encoding * (stmt list) option * bool * stmt list): unit =
        env.instructions <- Bindings.add x instr env.instructions

    let getDecoder (env: t) (x: ident): decode_case =
        Bindings.find x env.decoders

    let addDecoder (env: t) (x: ident) (d: decode_case): unit =
        env.decoders <- Bindings.add x d env.decoders

    let setImpdef (env: t) (x: string) (v: value): unit =
        env.impdefs <- ImpDefs.add x v env.impdefs

    let getImpdef (loc: l) (env: t) (x: string): value =
        (match ImpDefs.find_opt x env.impdefs with
        | Some v -> v
        | None ->
                raise (SymbolicError (loc, "Unknown value for IMPLEMENTATION_DEFINED \""^x^"\""))
        )

    (* CUSTOM WORK BELOW *)

(*    let rec push_ret (p: prog) (e: prog): prog =
      match List.rev p with
      | (Stmt_ProcReturn _)::xs -> p
      | (Stmt_If (g,tb,r,fb,loc))::xs -> List.rev ((Stmt_If (g,push_ret tb e,r,push_ret fb e,loc))::xs)
      | _ -> p @ e *)

    (* Add instructions to the residual program *)
    let push (p: prog) (env: t): unit =
      env.residual <- push env.residual p

    (* Generate a fresh variable name *)
    let freshName (env: t): ident =
      let i = ! (env.numSymbols) in 
      env.numSymbols := i + 1;
      Ident ("#"^ string_of_int i)

    (* Add a global constant *)
    let addGlobalConst (env: t) (x: ident) (v: value): unit =
      let v = subst_expr (EVar x) v in
      set_scope x (v) env.constants

    (* Add a global variable, replacing any expressions with accesses to the provided identifier *)
    let addGlobalVar (env: t) (x: ident) (v: value): unit =
      let v = subst_expr (EVar x) v in
      set_scope x (v) env.globals

    (* Add a local variable, don't stash it until necessary *)
    let addLocalVar (loc: l) (env: t) (x: ident) (v: value): unit =
      if !trace_write then Printf.printf "TRACE: decl local %s = %s\n" (pprint_ident x) (pp_value v);
      match env.locals with
      | (bs :: _) -> set_scope x (v) bs
      | []        -> raise (SymbolicError (loc, "addLocalVar: no scopes available"))

    (* Add a local constant, don't stash it until necessary *)
    let addLocalConst (loc: l) (env: t) (x: ident) (v: value): unit =
      if !trace_write then Printf.printf "TRACE: decl constlocal %s = %s\n" (pprint_ident x) (pp_value v);
      match env.locals with
      | (bs :: _) -> set_scope x (v) bs
      | []        -> raise (SymbolicError (loc, "addLocalConst: no scopes available"))

    (* Load a global constant *)
    let getGlobalConst (env: t) (x: ident): value =
      get_scope_val x env.constants

    (* Search for a local definition of a variable *)
    let findLocal (env: t) (x: ident): scope option =
      let rec search (bss : scope list): scope option =
        match bss with
        | (bs :: bss') -> if mem_scope x bs then Some bs else search bss'
        | [] -> None
      in
      search env.locals

    (** Write a value to an lexpr, flattening the operation down to individual scalar writes where possible *)
    let rec assignLExpr (loc: l) (x: AST.lexpr) (o: value) (v: value) (env: t): unit =
      match o, v with
      | VTuple os, VTuple vs -> unsupported loc "assignLExpr: can't expand tuple assignment"
      | VRecord os, VRecord vs -> 
          let _ = Bindings.merge (fun k v1 v2 ->
            match v1, v2 with
            | Some o, Some v -> Some (assignLExpr loc (LExpr_Field(x,k)) o v env)
            | _ -> symerror loc "updateWrap: Assignment between records of different shapes") os vs in
          ()
      | VArray (os,od), VArray (vs,d) ->
          let _ = Primops.ImmutableArray.merge (fun k v1 v2 ->
            let e = LExpr_Array(x,int_expr k) in
            match v1, v2 with
            | Some o, Some v -> Some (assignLExpr loc e o v env)
            | Some o, None   -> Some (assignLExpr loc e o (d) env)
            | None, Some v   -> Some (assignLExpr loc e (od) v env)
            | _ -> None) os vs in
          ()
      | _ -> 
          if v = o then ()
          else push [AST.Stmt_Assign(x, to_expr loc v, loc)] env

    (** Update stashed temporaries with a new value, given its old *)
    let rec assignTemps (loc: l) (x: value) (v: value) (env: t): value =
      match x, v with
      | VTuple xs, VTuple vs -> 
          VTuple (List.map2 (fun x v -> assignTemps loc x v env) xs vs)
      | VRecord xs, VRecord vs -> 
          VRecord (Bindings.merge (fun k v1 v2 ->
            match v1, v2 with
            | Some w, Some v -> Some (assignTemps loc w v env)
            | _ -> symerror loc "updateWrap: Assignment between records of different shapes") xs vs)
      | VArray (ws,wd), VArray (vs,d) ->
          VArray (Primops.ImmutableArray.merge (fun k v1 v2 ->
            match v1, v2 with
            | Some w, Some v -> Some (assignTemps loc w v env)
            | Some w, None   -> Some (assignTemps loc w (d ) env)
            | None, Some v   -> Some (assignTemps loc (wd) v env)
            | _ -> None) ws vs,wd)
      | _ -> 
            if x = v then v
            else
              let n = freshName env in
              push [AST.Stmt_VarDecl(to_type loc v, n, to_expr loc v, loc)] env;
              subst_expr (EVar n) v

    (* Merge two values to a single reference, potentially by stashing its value *)
    let rec mergeValue (loc: l) (decl: t) (e1: t) (e2: t) (w1: value) (w2: value): value =
      match w1, w2 with
      | VTuple bs1, VTuple bs2 -> VTuple (List.map2 (mergeValue loc decl e1 e2) bs1 bs2)
      | VRecord f1, VRecord f2 -> VRecord (Bindings.mapi (fun k v -> mergeValue loc decl e1 e2 v (Bindings.find k f2)) f1)
      | VArray (a1,d1), VArray (a2,d2) -> 
          VArray (Primops.ImmutableArray.merge (fun k v1 v2 ->
            match v1, v2 with
            | Some v1, Some v2 -> Some (mergeValue loc decl e1 e2 v1 v2)
            | Some v1, None -> Some (mergeValue loc decl e1 e2 v1 (d2))
            | None, Some v2 -> Some (mergeValue loc decl e1 e2 (d1) v2)
            | _ -> None) a1 a2, d1)
      | _ ->
          if w1 = w2 then w1
          else 
            let n = freshName decl in
            decl.decls <- decl.decls @ [Stmt_VarDeclsNoInit(to_type loc w1, [n], loc)];
            push [AST.Stmt_Assign(LExpr_Var n, to_expr loc w1, loc)] e1;
            push [AST.Stmt_Assign(LExpr_Var n, to_expr loc w2, loc)] e2;
            subst_expr (EVar n) w1

    let setLocalVar (loc: l) (env: t) (x: ident) (v: value) (s: scope): unit =
      let o = get_scope x s in
      match v with (* break aggregates down into scalars *)
      | VTuple _ 
      | VRecord _ 
      | VArray _ -> set_scope x (assignTemps loc o v env) s
      | _ -> set_scope x v s

    let setGlobalVar (loc: l) (env: t) (x: ident) (v: value): unit =
      let o = get_scope_val x env.globals in
      assignLExpr loc (LExpr_Var x) o v env; 
      set_scope x (v) env.globals
      (* reassignment ??? *)

    let setVar (loc: l) (env: t) (x: ident) (v: value): unit =
      if !trace_write then Printf.printf "TRACE: write %s = %s\n" (pprint_ident x) (pp_value v);
      match findLocal env x with
      | Some bs -> setLocalVar loc env x v bs
      | None ->
          if mem_scope x env.globals then setGlobalVar loc env x v
          else raise (SymbolicError (loc, "setVar: " ^ pprint_ident x))

    let getVar (loc: l) (env: t) (x: ident): value =
      match findLocal env x with
      | Some bs -> get_scope_val x bs
      | None ->
          if mem_scope x env.globals then get_scope_val x env.globals 
          else if mem_scope x env.constants then get_scope_val x env.constants
          else raise (SymbolicError (loc, "getVar: " ^ pprint_ident x))

    (* Access the residual program for this state *)
    let residual (env: t): prog = env.decls (* @ env.residual *)

    (* Join the variables of two states *)
    let mergeVars (loc: l) (decl: t) (e1: t) (e2: t): unit =
      let merge l r: scope =
        {
          bs = Bindings.merge (fun k v1 v2 ->
            match v1, v2 with
            | Some v1, Some v2 -> Some (mergeValue loc decl e1 e2 v1 v2)
            | Some v, None -> Some v
            | None, Some v -> Some v
            | _ -> None) l.bs r.bs
        }
      in
      decl.locals <- List.map2 merge e1.locals e2.locals;
      decl.globals <- merge e1.globals e2.globals

      (*
    let rec createRet (loc: l) (v: value) (e: t): ret =
      match v with
      | VTuple vs -> ATuple (List.map (fun v -> createRet loc v e) vs)
      | VRecord vs -> ARecord (Bindings.map (fun v -> createRet loc v e) vs)
      | VArray (vs,d) -> failwith ""
      | _ -> 
          (let n = freshName e in
          e.decls <- e.decls @ [Stmt_VarDeclsNoInit(to_type v, [n], loc)];
          push [AST.Stmt_Assign(LExpr_Var n, to_expr loc v, loc)] e;
          At n)

    let rec stashRet (loc: l) (r: ret) (v: value) (e: t): unit =
      match r, v with
      | ATuple xs, VTuple vs      -> List.iter2 (fun x v -> stashRet loc x v e) xs vs
      | ARecord xs, VRecord vs    -> Bindings.iter (fun k x -> stashRet loc x (Bindings.find k vs) e) xs
      | At i, _                   -> push [AST.Stmt_Assign(LExpr_Var i, to_expr loc v, loc)] e
      | _                 -> failwith ""

    let stashResult (loc: l) (v: value) (e: t): unit =
      match e.returnSyms with
      | NoDecl::xs -> 
          e.returnSyms <- (createRet loc v e)::xs
      | x::xs -> stashRet loc x v e
      | [] -> raise (SymbolicError (loc, "stashResult")) 

*)

end

(****************************************************************)
(** {2 Abstract Interpretation }                                *)
(****************************************************************)

module Disem = Abstract.Make(
struct
  type t = value

  (* Value Constructors *)
  let mk_bool (x: bool): value                  = VBool (Left x)
  let mk_int (x: int): value                    = VInt (Left (Z.of_int x))
  let mk_bigint (x: Z.t): value                 = VInt (Left x)
  let mk_real (x: Q.t): value                   = VReal (Left x)
  let mk_bits (n: int) (v: Z.t): value          = VBits (Left (Primops.mkBits n v))
  let mk_mask (n: int) (v: Z.t) (m: Z.t): value = VMask (Left (Primops.mkMask n v m))
  let mk_string (x: string): value              = VString (Left x)

  let from_enum x y      : value = VInt (Left (Z.of_int y))
  let from_exc x y       : value = VExc (x,y)
  let from_tuple l       : value = VTuple l

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
  let to_exc (loc: AST.l) (x: value): (AST.l * Primops.exc) =
    match x with
    | VExc v -> v
    | _ -> symerror loc @@ "exception expected. Got " ^ pp_value x

  (* Unit *)
  let unit = VTuple []
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
)(
struct
  type value = Symbolic.value
  type 'a eff = Env.t -> ('a, exn) Either.t

  exception Return    of value

  (* Monadic *)
  let pure (a: 'a): 'a eff = 
    fun _ -> Either.Left (a)
  let (>>) (a: 'a eff) (f: 'a -> 'b): 'b eff = 
    fun e ->
    match a e with
    | Either.Left v -> Either.Left (f v)
    | Either.Right e -> Either.Right e
  let (>>=) (a: 'a eff) (f: 'a -> 'b eff): 'b eff = 
    fun e -> 
    match a e with
    | Either.Left v -> f v e
    | Either.Right e -> Either.Right e

  (* State *)
  let reset = pure () (* TODO: something *)
  let scope b = 
    fun e ->
      let (r,p) = Env.nest b e in
      Env.push p e;
      r
  let call (b : unit eff): value eff =
    fun e -> (* need the body for the callee *)
      let (r,p) = Env.nestTop b e in
      Env.push p e;
      match r with
      | Either.Left () -> Either.Left (VTuple [])
      | Either.Right (Return v) -> Left v
      | Either.Right x -> Right x

  let runPrim f tes es = 
    match sym_pure_prim f tes es with
    | Some x -> pure (Some x)
    | None -> pure None
    (* TODO: non-pure primitive functions *)

  let isGlobalConstFilter =
    fun env ->
      Either.Left (fun t ->
        match Env.getGlobalConst env t with
        | _ -> true
        | exception _ -> false)

  let wrap e = Either.Left (let () = e in ())

  (* add these to the state, but don't add anything to residual *)
  let addLocalVar   (loc: l) (x: ident) (v: value) (env: Env.t)     = wrap (Env.addLocalVar   loc env x v)
  let addLocalConst (loc: l) (x: ident) (v: value) (env: Env.t)     = wrap (Env.addLocalConst loc env x v)
  let addGlobalVar (x: ident) (v: value) (env: Env.t)               = wrap (Env.addGlobalVar env x v)

  let setVar (loc: l) (x: ident) (v: value) (env: Env.t)            = wrap (Env.setVar loc env x v)

  let getVar (loc: l) (x: ident) (env: Env.t)                       = Either.Left (Env.getVar loc env x)

  (* immutable state *)
  let addGlobalConst (x: ident) (v: value) (env: Env.t)             = wrap (Env.addGlobalConst env x v)
  let getGlobalConst (x: ident) (env: Env.t)                        = Either.Left (Env.getGlobalConst env x)

  let addEnum (x: ident) (vs: value list) (env: Env.t)              = wrap (Env.addEnum env x vs)
  let getEnum (x: ident) (env: Env.t)                               = Either.Left (Env.getEnum env x)

  let addRecord (x: ident) (vs: (AST.ty * ident) list) (env: Env.t) = wrap (Env.addRecord env x vs)
  let getRecord (x: ident) (env: Env.t)                             = Either.Left (Env.getRecord env x)

  let addTypedef (x: ident) (t: AST.ty) (env: Env.t)                = wrap (Env.addTypedef env x t)
  let getTypedef (x: ident) (env: Env.t)                            = Either.Left (Env.getTypedef env x)

  let addFun (loc: l) (x: ident) (f: fun_sig) (env: Env.t)          = wrap (Env.addFun loc env x f)
  let getFun (loc: l) (x: ident) (env: Env.t)                       = Either.Left (Env.getFun loc env x)

  let addInstruction (loc: l) (x: ident) (i: inst_sig) (env: Env.t) = wrap (Env.addInstruction loc env x i)
  let getInstruction (loc: l) (x: ident) (env: Env.t)               = Either.Left (Env.getInstruction loc env x)

  let addDecoder (x: ident) (d: decode_case) (env: Env.t)           = wrap (Env.addDecoder env x d)
  let getDecoder (x: ident) (env: Env.t)                            = Either.Left (Env.getDecoder env x)

  let getImpdef (loc: l) (s: string) (env: Env.t)                   = Either.Left (Env.getImpdef loc env s)

  (* Control Flow *)
  let branch (c: value) (t: value eff lazy_t) (f: value eff lazy_t) = 
    let loc = Unknown in 
    fun e ->
    match to_bool loc c with
    | (Left true) -> Lazy.force t e
    | (Left false) -> Lazy.force f e
    | (Right exp) -> 
        let te = Env.copy e in
        let fe = Env.copy e in
        let tr = Lazy.force t te in
        let fr = Lazy.force f fe in
        match tr, fr with
        | Left v1, Left v2 when v2 = v1 -> 
            Env.mergeVars loc e te fe;
            Env.push [AST.Stmt_If (lift_expr loc exp,Env.residual te,[],Env.residual fe,loc) ] e;
            Left v1 
        | Left v1, Left v2 ->
            let v = Env.mergeValue loc e te fe v1 v2 in
            Env.mergeVars loc e te fe;
            Env.push [AST.Stmt_If (lift_expr loc exp,Env.residual te,[],Env.residual fe,loc) ] e;
            Left v
        | Left v, Right (Return r) ->
            (* Env.stashResult loc r fe; *)
            Env.push [AST.Stmt_If (lift_expr loc exp,Env.residual te,[],Env.residual fe@[AST.Stmt_ProcReturn(loc)],loc) ] e;
            (*Env.copy_vars e te;*)
            Left v
        | _ -> raise (SymbolicError (Unknown, "symbolic if")) 

  let iter (b: value -> (value * value) eff) (s: value): value eff =
    raise (SymbolicError (Unknown,"loop"))

  let catch (b: 'a eff) (f: AST.l -> Primops.exc -> 'a eff): 'a eff = 
    fun e ->
      match b e with
      | Right (Value.Throw(l, x)) -> f l x e
      | x -> x

  let return v: 'a eff = 
    fun _ -> Either.Right (Return v)

  let throw l x = 
    fun e -> 
      Env.push [AST.Stmt_Assert (bool_expr false, l)] e;
      Either.Right (Throw (l,x))

  let error l s = 
    fun _ -> raise (SymbolicError (l,s))

end)

let eval_decode_case (loc: AST.l) (env: Env.t) (d: AST.decode_case) (v: value): prog =
  match Disem.eval_decode_case loc d v env with
  | Left () -> Env.residual env
  | Right e -> raise e

let build_evaluation_environment (defs: AST.declaration list): Env.t =
  let e = Env.empty () in
  match Disem.build_evaluation_environment defs e with
  | Left () -> e
  | Right x -> raise x

(****************************************************************
 * End
 ****************************************************************)
