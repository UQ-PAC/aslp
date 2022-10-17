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

let int_expr (i: int): AST.expr = AST.Expr_LitInt (string_of_int i)

let bool_expr (b: bool): AST.expr = AST.Expr_Var(if b then Ident "TRUE" else Ident "FALSE")

let sym_pure_prim (f: string) (tvs: value list) (vs: value list): value option =
let open Primops in
let f' : type a. a Symbolic.sym = Either.Right (ECall (FIdent (f,0), tvs, vs)) in
let b' : vint -> vbits = fun n -> Right {n=n; v=ECall (FIdent (f,0), tvs, vs)} in
( match (f, tvs, vs) with
| ("add_int",           [      ], [VInt  x; VInt  y    ])     -> Some (VInt    (sym_add_int    x y))
| ("cvt_bits_uint",     [VInt n], [VBits x             ])     -> Some (VInt    (sym_cvt_bits_uint x))
| ("cvt_bits_sint",     [VInt n], [VBits x             ])     -> Some (VInt    (sym_cvt_bits_sint x))
| ("eq_int",            [      ], [VInt  x; VInt  y    ])     -> Some (VBool   (sym_eq_int x y))

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
| ("sub_int",           [      ], [VInt  (Left x); VInt  (Left y)    ])     -> Some (VInt    (Left(prim_sub_int    x y)))
| ("shl_int",           [      ], [VInt  (Left x); VInt  (Left y)    ])     -> Some (VInt    (Left(prim_shl_int    x y)))
| ("shr_int",           [      ], [VInt  (Left x); VInt  (Left y)    ])     -> Some (VInt    (Left(prim_shr_int    x y)))
| ("mul_int",           [      ], [VInt  (Left x); VInt  (Left y)    ])     -> Some (VInt    (Left(prim_mul_int    x y)))
| ("zdiv_int",          [      ], [VInt  (Left x); VInt  (Left y)    ])     -> Some (VInt    (Left(prim_zdiv_int   x y)))
| ("zrem_int",          [      ], [VInt  (Left x); VInt  (Left y)    ])     -> Some (VInt    (Left(prim_zrem_int   x y)))
| ("fdiv_int",          [      ], [VInt  (Left x); VInt  (Left y)    ])     -> Some (VInt    (Left(prim_fdiv_int   x y)))
| ("frem_int",          [      ], [VInt  (Left x); VInt  (Left y)    ])     -> Some (VInt    (Left(prim_frem_int   x y)))
| ("mod_pow2_int",      [      ], [VInt  (Left x); VInt  (Left y)    ])     -> Some (VInt    (Left(prim_mod_pow2_int x y)))
| ("align_int",         [      ], [VInt  (Left x); VInt  (Left y)    ])     -> Some (VInt    (Left(prim_align_int    x y)))
| ("pow2_int",          [      ], [VInt  (Left x)             ])     -> Some (VInt    (Left(prim_pow2_int     x)))
| ("pow_int_int",       [      ], [VInt  (Left x); VInt  (Left y)    ])     -> Some (VInt    (Left(prim_pow_int_int  x y)))
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
| ("round_down_real",   [      ], [VReal (Left x)             ])     -> Some (VInt    (Left(prim_round_down_real x)))
| ("round_up_real",     [      ], [VReal (Left x)             ])     -> Some (VInt    (Left(prim_round_up_real x)))
| ("sqrt_real",         [      ], [VReal (Left x); VReal (Left y)    ])     -> Some (VReal   (Left(prim_sqrt_real x)))
| ("sqrt_real",         [      ], [VReal _       ; VReal _           ])     -> Some (VReal   (f'))
| ("cvt_int_bits",      [_     ], [VInt  (Left x); VInt  (Left n)    ])     -> Some (VBits   (Left(prim_cvt_int_bits n x)))
| ("cvt_int_bits",      [VInt n], [VInt  _       ; VInt  _           ])     -> Some (VBits   (b' n))


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
  | Return of (stmt list * 'a)
  | Throw  of (stmt list)
  | Branch of (bool * stmt list * expr * 'a resid * 'a resid)

let contains_open res =
  match res with 
  | Open _ -> true 
  | Branch (b,_,_,_,_) -> b
  | _ -> false

let map_open (f: stmt list -> 'a resid) (res: 'a resid): 'a resid =
  let rec loop res =
    match res with
    | Open s -> 
        let r = f s in (contains_open r,r)
    | Branch (true,s,c,l,r) -> 
        let (lf,ls) = loop l in let (rf,rs) = loop r in
        (lf || rf, Branch (lf || rf,s,c,ls,rs))
    | _ -> (false,res)
  in
  let (_,out) = loop res in out

let push (res: 'a resid) (ps: stmt list): 'a resid =
  map_open (fun s -> Open (s @ ps)) res

let push_return (res: 'a resid) (a: 'a): 'a resid =
  map_open (fun s -> Return (s, a)) res

let push_throw (res: 'a resid): 'a resid =
  map_open (fun s -> Throw s) res

let push_branch (loc: l) (res: 'a resid) (c: expr) (l: 'a resid) (r: 'a resid) =
  match l, r with
  | Open l, Open r -> push res [AST.Stmt_If (lift_expr loc c,l,[],r,loc)]
  | _ -> map_open (fun s -> Branch(contains_open l || contains_open r,s,c,l,r)) res

let rec resid_to_stmts (loc: l) (r: 'a -> stmt list) (res: 'a resid): stmt list =
  match res with
  | Open s -> s
  | Return (s,v) -> s @ (r v)
  | Throw s -> s @ [AST.Stmt_Assert (bool_expr false,loc)]
  | Branch (_,s,c,ls,rs) -> s @ [AST.Stmt_If (lift_expr loc c,resid_to_stmts loc r ls,[],resid_to_stmts loc r rs,loc)]

(****************************************************************)
(** {2 Stashed Value}                                           *)
(****************************************************************)

(** A partially stashed value, with expansion for aggregates *)
type stashed = 
  | SVal of value
  | SIdent of ident * value
  | STuple of stashed list 
  | SRecord of stashed Bindings.t 
  | SArray of (stashed Primops.ImmutableArray.t * value)

(** Extract a value from the stashed representation *)
let rec value_of_stashed (s: stashed): value =
  match s with
  | SVal v -> v
  | SIdent (i,v) -> copy_scalar_type (EVar i) v
  | STuple ts -> VTuple (List.map value_of_stashed ts)
  | SRecord fs -> VRecord (Bindings.map value_of_stashed fs)
  | SArray (a,d) -> VArray (Primops.ImmutableArray.map value_of_stashed a, d)

(** Build a unstashed value from some initial value *)
let rec stashed_of_value (v: value): stashed =
  match v with
  | VTuple ts -> STuple (List.map stashed_of_value ts)
  | VRecord fs -> SRecord (Bindings.map stashed_of_value fs)
  | VArray (a,d) -> SArray (Primops.ImmutableArray.map stashed_of_value a, d)
  | v -> SVal v 

let array_merge (f: 'a -> 'b -> 'c) (dl: 'k -> 'a) (dr: 'k -> 'b) k (a: 'a option) (b: 'b option): 'c option =
  match a, b with
  | Some a', Some b' -> Some (f a' b')
  | Some a', None    -> Some (f a' (dr k))
  | None, Some b'    -> Some (f (dl k) b')
  | _ -> None

(** Create new stashed variables *)
let rec merge_stashed_value (f: value -> ident) (s: stashed) (v: value): stashed =
  let loop s v = merge_stashed_value f s v in
  let loop' _ s v = Some (merge_stashed_value f s v) in
  match s, v with
  | STuple ts, VTuple vs -> STuple (List.map2 loop ts vs)
  | SRecord fs, VRecord vs -> SRecord (record_merge loop' fs vs)
  | SArray (a,ds), VArray (vs,v) -> 
      assert (ds = v);
      SArray (Primops.ImmutableArray.merge (array_merge loop (fun k -> SVal (array_default ds k)) (array_default v)) a vs, ds)
  | SIdent (i,v), _ -> SIdent (i,v)
  | SVal v', _ -> if v' = v then s else let i = f v' in SIdent (i, v)
  | _ -> invalid_arg "merge of different aggregate types"

(* TODO: map fold rather than this mess *)
let assign_stashed (loc: l) (s: stashed) (v: value): stmt list =
  let out = ref [] in
  let rec loop s v = match s, v with
    | STuple ts, VTuple vs -> List.iter2 loop ts vs 
    | SRecord fs, VRecord vs -> let _ = record_merge (fun _ a b -> Some (loop a b)) fs vs in ()
    | SArray (a,ds), VArray (vs,v) -> 
        assert (ds = v);
        let _ = (Primops.ImmutableArray.merge (array_merge loop 
          (fun k -> SVal (array_default ds k)) (array_default v)) a vs, ds)
        in ()
    | SIdent (i,_), _ -> 
        out := AST.Stmt_Assign(LExpr_Var i, to_expr loc v, loc)::!out
    | SVal _, _ -> ()
    | _ -> failwith "assignStashed: unknown case"
  in
  loop s v;
  !out

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

let rec find_scope (bss: scope list) (x: ident): scope option =
  match bss with
  | (bs :: bss') -> if mem_scope x bs then Some bs else find_scope bss' x
  | [] -> None


(****************************************************************)
(** {2 Mutable bindings}                                        *)
(****************************************************************)

type prog = AST.stmt list

(** Environment representing both global and local state of the system *)
module Env : sig
    type t

    val empty               : unit -> t
    val copy                : t -> t

    val instScope           : (t -> 'a) -> (t -> 'a * prog)
    val funScope            : (t -> 'a) -> (t -> 'a * value)
    val innerScope          : (t -> 'a) -> (t -> 'a)

    val addLocalVar         : AST.l -> t -> ident -> value -> unit
    val addLocalConst       : AST.l -> t -> ident -> value -> unit

    val addGlobalConst      : t -> ident -> value -> unit
    val getGlobalConst      : ident -> t -> value

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

    val return              : AST.l -> value -> t -> unit
    val throw               : AST.l -> Primops.exc -> t -> unit

    val residual            : AST.l -> t -> stmt list
    val mergeState          : AST.l -> expr -> t -> t -> t -> unit
    val mergeValue          : AST.l -> t -> t -> value -> value -> value

end = struct
  type t = {
    (* Shared across instructions *)
    mutable instructions : (encoding * (stmt list) option * bool * stmt list) Bindings.t;
    mutable decoders     : decode_case Bindings.t;
    mutable functions    : fun_sig Bindings.t;
    mutable enums        : (value list) Bindings.t;
    mutable records      : ((AST.ty * ident) list) Bindings.t;
    mutable typedefs     : AST.ty Bindings.t;
    mutable impdefs      : value ImpDefs.t;
    mutable constants    : scope;
    mutable globalnames  : scope;

    (* Shared within instruction *)
    mutable globals      : scope;
    mutable numSymbols   : int ref;
    mutable decls        : AST.stmt list ref;

    (* Shared within function *)
    mutable locals       : scope list;
    mutable returnSyms   : stashed option ref;

    (* Shared within scope *)
    mutable residual     : value resid;
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
    | None -> symerror loc  @@ "Unknown value for IMPLEMENTATION_DEFINED \""^x^"\""
    )

  (** Add instructions to the residual program *)
  let push (p: prog) (env: t): unit =
    env.residual <- push env.residual p

  let inner_residual (loc: l) (env: t): stmt list =
    match !(env.returnSyms) with
    | Some r -> resid_to_stmts loc (assign_stashed loc r) env.residual
    | None -> resid_to_stmts loc (fun _ -> []) env.residual

  let residual (loc: l) (env: t): stmt list =
    !(env.decls) @ inner_residual loc env

  (** Write a value to an lexpr, flattening the operation down to individual scalar writes if possible *)
  let rec assignLExpr (loc: l) (x: AST.lexpr) (o: value) (v: value) (env: t): unit =
    match o, v with
    | VTuple os, VTuple vs ->
        unsupported loc "assignLExpr: can't expand tuple assignment"
    | VRecord os, VRecord vs ->
        let upd k v1 v2 = Some (assignLExpr loc (LExpr_Field(x,k)) v1 v2 env) in
        let _ = record_merge upd os vs in
        ()
    | VArray (os,od), VArray (vs,d) ->
        let upd k v1 v2 = Some (assignLExpr loc (LExpr_Array(x,int_expr k)) v1 v2 env) in
        let _ = Symbolic.array_merge upd os od vs d in
        ()
    | _ -> if v != o then push [AST.Stmt_Assign(x, to_expr loc v, loc)] env

  let updateGlobals (env: t): unit =
    Bindings.iter (fun k v ->
      let prev = get_scope k env.globalnames in
      assignLExpr Unknown (LExpr_Var k) prev v env
    ) env.globals.bs

(****************************************************************
 * Scopes
 ****************************************************************)

  let empty _ = {
    instructions = Bindings.empty;
    decoders     = Bindings.empty;
    functions    = Bindings.empty;
    enums        = Bindings.empty;
    records      = Bindings.empty;
    typedefs     = Bindings.empty;
    impdefs      = ImpDefs.empty;
    constants    = empty_scope ();
    globalnames  = empty_scope ();

    globals      = empty_scope ();
    locals       = [empty_scope ()];
    residual     = Open [];
    decls        = ref [];
    returnSyms   = ref None;
    numSymbols   = ref 0;
  }

  (** Setup structures that are shared across an instruction *)
  let instScope (k: t -> 'a) (parent: t): 'a * prog =
    let child = {
      decoders     = parent.decoders;
      instructions = parent.instructions;
      functions    = parent.functions;
      enums        = parent.enums;
      records      = parent.records;
      typedefs     = parent.typedefs;
      globalnames  = parent.globalnames;
      constants    = parent.constants;
      impdefs      = parent.impdefs;

      (* changes *)
      globals      = empty_scope();
      decls        = ref [];
      numSymbols   = ref 0;
      locals       = [empty_scope ()];
      residual     = Open [];
      returnSyms   = ref None;
    } in
    let r = k child in
    updateGlobals child;
    (r, residual Unknown child)

  (** Create an environment for a callee and return 
      their result along with a residual program *)
  let funScope (k: t -> 'a) (parent: t): 'a * value =
    let child = {
      decoders     = parent.decoders;
      instructions = parent.instructions;
      functions    = parent.functions;
      enums        = parent.enums;
      records      = parent.records;
      typedefs     = parent.typedefs;
      globalnames  = parent.globalnames;
      globals      = parent.globals;
      constants    = parent.constants;
      impdefs      = parent.impdefs;
      numSymbols   = parent.numSymbols;
      decls        = parent.decls;

      (* changes *)
      locals       = [empty_scope ()];
      residual     = Open [];
      returnSyms   = ref None;
    } in
    (* Run the child *)
    let r = k child in
    (* TODO: rather than refs, can we pull the shared items out? *)
    (* copy out: numSymbols, new decls, etc. *)
    push (inner_residual Unknown child) parent;
    match !(child.returnSyms) with
    | Some v -> 
        (r, value_of_stashed v)
    | None -> 
        (r, VTuple []) (* TODO: Value.unit *)

  (** Create a nested scope within a function and return
      the scope's result along with its residual program *)
  let innerScope (k: t -> 'a) (parent: t): 'a =
    let child = {
      decoders     = parent.decoders;
      instructions = parent.instructions;
      functions    = parent.functions;
      enums        = parent.enums;
      records      = parent.records;
      typedefs     = parent.typedefs;
      globalnames  = parent.globalnames;
      globals      = parent.globals;
      constants    = parent.constants;
      impdefs      = parent.impdefs;
      numSymbols   = parent.numSymbols;
      decls        = parent.decls;
      returnSyms   = parent.returnSyms;

      (* changes *)
      locals       = empty_scope () :: parent.locals;
      residual     = Open [];
    } in
    let r = k child in
    push (inner_residual Unknown child) parent;
    r

  (* TODO: something like 'revertible': 
        run a command on a new state, but hide all of its effects and return them somehow.
        would be nice if this returned just (globals * locals * residual * command result)
        then the argument that only these have to be merged is obvious

  let (tres,tchg) = Env.revertible tbranch e in
  let (fres,fchg) = Env.revertible fbranch e in
  let changes = Env.mergeChanges tchg fchg in
  Env.applyChanges changes env

        *)
  let copy (env: t): t = {
      decoders     = env.decoders;
      instructions = env.instructions;
      functions    = env.functions;
      enums        = env.enums;
      records      = env.records;
      typedefs     = env.typedefs;
      constants    = env.constants;
      impdefs      = env.impdefs;
      numSymbols   = env.numSymbols;
      returnSyms   = env.returnSyms;
      decls        = env.decls;
      globalnames  = env.globalnames;

      (* changes *)
      globals      = { bs = env.globals.bs };
      locals       = List.map (fun x -> { bs = x.bs }) env.locals;
      residual     = Open [];
    }

  (** Generate a fresh temporary *)
  let newTemp (loc: l) (v: value) (env: t): ident =
    let i = ! (env.numSymbols) in
    let n = Ident ("#"^string_of_int i) in
    env.numSymbols := i + 1;
    env.decls := !(env.decls) @ [Stmt_VarDeclsNoInit(to_type loc v, [n], loc)];
    n

  (** Load a global constant, no special handling due to constant *)
  let getGlobalConst (x: ident) (env: t) : value =  
    get_scope_val x env.constants

  (** Get any variable, prefering recently defined locals to globals *)
  let getVar (loc: l) (env: t) (x: ident): value =
    match find_scope env.locals x with
    | Some bs -> get_scope_val x bs
    | None ->
        if mem_scope x env.globals then get_scope_val x env.globals
        else if mem_scope x env.globalnames then get_scope_val x env.globalnames
        else if mem_scope x env.constants then get_scope_val x env.constants
        else symerror loc @@ "getVar: Couldn't find " ^ pprint_ident x

  (* Add a global constant *)
  let addGlobalConst (env: t) (x: ident) (v: value): unit =
    let v = subst_base (EVar x) v in
    set_scope x v env.constants

  (* Add a global variable, replacing any expressions with accesses to the provided identifier *)
  let addGlobalVar (env: t) (x: ident) (v: value): unit =
    let v = subst_base (EVar x) v in
    set_scope x v env.globalnames

  (* Add a local variable, don't stash it until necessary *)
  let addLocalVar (loc: l) (env: t) (x: ident) (v: value): unit =
    if !trace_write then Printf.printf "TRACE: decl local %s = %s\n" (pprint_ident x) (pp_value v);
    match env.locals with
    | (bs :: _) -> set_scope x (v) bs
    | []        -> symerror loc "addLocalVar: no scopes available"

  (* Add a local constant, don't stash it until necessary *)
  let addLocalConst (loc: l) (env: t) (x: ident) (v: value): unit =
    if !trace_write then Printf.printf "TRACE: decl constlocal %s = %s\n" (pprint_ident x) (pp_value v);
    match env.locals with
    | (bs :: _) -> set_scope x (v) bs
    | []        -> symerror loc "addLocalConst: no scopes available"

  (* Set a local variable, currently just sets it, nothing else to worry about *)
  (* TODO: hook for logic handling stashing of complex local expressions *)
  let setLocalVar (loc: l) (env: t) (x: ident) (v: value) (s: scope): unit =
    set_scope x v s

  (* Set a global variable and add effects to the residual program *)
  let setGlobalVar (loc: l) (env: t) (x: ident) (v: value): unit =
    (*let prev = (match get_scope_opt x env.globals with
    | Some v -> v
    | None -> get_scope x env.globalnames)
    in
    assignLExpr loc (LExpr_Var x) prev v env; *)
    set_scope x v env.globals

  (* Set a variable, either local or global *)
  let setVar (loc: l) (env: t) (x: ident) (v: value): unit =
    if !trace_write then Printf.printf "TRACE: write %s = %s\n" (pprint_ident x) (pp_value v);
    match find_scope env.locals x with
    | Some bs -> setLocalVar loc env x v bs
    | None ->
        if mem_scope x env.globalnames then setGlobalVar loc env x v
        else symerror loc @@ "setVar: Couldn't find " ^ pprint_ident x

  (** Merge two values to a single reference, potentially by stashing its value *)
  let mergeValue (loc: l) (e1: t) (e2: t) (v1: value) (v2: value): value =
    let s = stashed_of_value v1 in
    let s' = merge_stashed_value (fun v -> newTemp loc v e1) s v2 in
    push (assign_stashed loc s' v1) e1;
    push (assign_stashed loc s' v2) e2;
    value_of_stashed s'

  (* Join the variables of two states *)
  let mergeState (loc: l) (e: expr) (orig: t) (e1: t) (e2: t): unit =
    let merge l r: scope =
      {
        bs = Bindings.merge (fun k v1 v2 ->
          match v1, v2 with
          | Some v1, Some v2 -> Some (mergeValue loc e1 e2 v1 v2)
          | Some v, None -> Some v
          | None, Some v -> Some v
          | _ -> None) l.bs r.bs
      }
    in
    orig.residual <- push_branch loc orig.residual e e1.residual e2.residual;
    match contains_open e1.residual, contains_open e2.residual with
    | true, true ->
        orig.locals <- List.map2 merge e1.locals e2.locals;
        orig.globals <- merge e1.globals e2.globals;
    | true, false -> 
        orig.locals <- e1.locals;
        orig.globals <- e1.globals;
    | false, true -> 
        orig.locals <- e2.locals;
        orig.globals <- e2.globals;
    | _ -> ()

  (* End disassembly due to a throw, we consider these unreachable *)
  let throw (loc: l) (x: Primops.exc) (env: t): unit =
    env.residual <- push_throw env.residual

  (* Exit a function by updating the return symbols and closing the residual program *)
  let return (loc: l) (v: value) (env: t): unit =
    env.returnSyms := (match !(env.returnSyms) with
    | Some s -> Some (merge_stashed_value (fun v -> newTemp loc v env) s v)
    | None -> Some (stashed_of_value v));
    env.residual <- push_return env.residual v
end

(****************************************************************)
(** {2 Abstract Interpretation }                                *)
(****************************************************************)

module Disem = Abstract.Make(
struct
  type t = value
  let pp_value = pp_value

  (* Value Constructors *)
  let mk_bool (x: bool): value                  = VBool (Left x)
  let mk_int (x: int): value                    = VInt (Left (Z.of_int x))
  let mk_bigint (x: Z.t): value                 = VInt (Left x)
  let mk_real (x: Q.t): value                   = VReal (Left x)
  let mk_bits (n: int) (v: Z.t): value          = VBits (Left (Primops.mkBits n v))
  let mk_mask (n: int) (v: Z.t) (m: Z.t): value = VMask (Left (Primops.mkMask n v m))
  let mk_string (x: string): value              = VString (Left x)
  let mk_enum x y      : value = VInt (Left (Z.of_int y))
  let mk_exc x y       : value = VExc (x,y)
  let mk_tuple l       : value = VTuple l

  (* Value Destructors *)
  let get_tuple (loc: AST.l) (x: value): value list =
    match x with
    | VTuple xs -> xs
    | _ -> symerror loc @@ "tuple expected. Got " ^ pp_value x
  let get_string (loc: AST.l) (x: value): string =
    match x with
    | VString (Left v) -> v
    | VString (Right e) ->
        unsupported loc @@ "can't convert symbolic string to concrete value. Got " ^ pp_value x
    | _ -> symerror loc @@ "string expected. Got " ^ pp_value x
  let get_exc (loc: AST.l) (x: value): (AST.l * Primops.exc) =
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
    VBool (sym_le_int (to_int loc x) (to_int loc y))

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
  let unknown_integer _   = VInt (Right {s=true; min=None; max=None; w=Z.zero; e=EUnknown})
  let unknown_real    _   = VReal (Right EUnknown)
  let unknown_string  _   = VString (Right EUnknown)
  let unknown_bits    l w = VBits (Right {n=to_int l w; v=EUnknown})
  let unknown_enum    _ _ = VInt (Right {s=true; min=None; max=None; w=Z.zero; e=EUnknown})
  let unknown_ram     _ _ = VRAM (Right EUnknown)

end
)(struct
  type value = Symbolic.value
  type 'a eff = Env.t -> 'a Option.t

  (* Monadic *)
  let pure (a: 'a): 'a eff = 
    fun _ -> Some a
  let (>>) (a: 'a eff) (f: 'a -> 'b): 'b eff = 
    fun e ->
    match a e with
    | Some v -> Some (f v)
    | None -> None
  let (>>=) (a: 'a eff) (f: 'a -> 'b eff): 'b eff = 
    fun e -> 
    match a e with
    | Some v -> f v e
    | None -> None

  (* Effectful primitives *)
  let runPrim f tes es = 
    match sym_pure_prim f tes es with
    | Some x -> pure (Some x)
    | None -> pure None
    (* TODO: non-pure primitive functions *)

  (* State *)
  let reset: unit eff = 
    fun e -> Some () (* TODO: needed? *)
  let scope (b: unit eff): unit eff = 
    fun e -> Env.innerScope b e

  (* State Reads - TODO: Merge this structure with env *)
  let getGlobalConst (x: ident) (env: Env.t) =
    Some (Env.getGlobalConst x env)
  let getVar (loc: l) (x: ident) (env: Env.t) = 
    Some (Env.getVar loc env x)
  let getImpdef (loc: l) (s: string) (env: Env.t) = 
    Some (Env.getImpdef loc env s)
  let getFun (loc: l) (x: ident) (env: Env.t) = 
    Some (Env.getFun loc env x)
  let getInstruction (loc: l) (x: ident) (env: Env.t) = 
    Some (Env.getInstruction loc env x)
  let getDecoder (x: ident) (env: Env.t) = 
    Some (Env.getDecoder env x)
  let getEnum (x: ident) (env: Env.t) = 
    Some (Env.getEnum env x)
  let getRecord (x: ident) (env: Env.t) = 
    Some (Env.getRecord env x)
  let getTypedef (x: ident) (env: Env.t) = 
    Some (Env.getTypedef env x)
  let isGlobalConstFilter (env: Env.t) =
    Some (fun t ->
      match Env.getGlobalConst t env with
      | _ -> true
      | exception _ -> false)

  (* State Writes *)
  let addRecord (x: ident) (vs: (AST.ty * ident) list) (env: Env.t) = 
    Some (Env.addRecord env x vs)
  let addGlobalConst (x: ident) (v: value) (env: Env.t) = 
    Some (Env.addGlobalConst env x v)
  let addGlobalVar (x: ident) (v: value) (env: Env.t) = 
    Some (Env.addGlobalVar env x v)
  let addEnum (x: ident) (vs: value list) (env: Env.t) = 
    Some (Env.addEnum env x vs)
  let addTypedef (x: ident) (t: AST.ty) (env: Env.t) = 
    Some (Env.addTypedef env x t)
  let addDecoder (x: ident) (d: decode_case) (env: Env.t) = 
    Some (Env.addDecoder env x d)
  let addInstruction (loc: l) (x: ident) (i: inst_sig) (env: Env.t) = 
    Some (Env.addInstruction loc env x i)
  let addFun (loc: l) (x: ident) (f: fun_sig) (env: Env.t) = 
    Some (Env.addFun loc env x f)
  let addLocalVar   (loc: l) (x: ident) (v: value) (env: Env.t) = 
    Some (Env.addLocalVar   loc env x v)
  let addLocalConst (loc: l) (x: ident) (v: value) (env: Env.t) = 
    Some (Env.addLocalConst loc env x v)
  let setVar (loc: l) (x: ident) (v: value) (env: Env.t) = 
    Some (Env.setVar loc env x v)

  (* Control Flow *)
  let branch (c: value) (t: value eff) (f: value eff) = 
    let loc = Unknown in 
    fun e ->
      match to_bool loc c with
      | (Left true) -> t e
      | (Left false) -> f e
      | (Right exp) -> 
          let te = Env.copy e in
          let fe = Env.copy e in
          let r = (match t te, f fe with
          | Some v1, Some v2 -> Some (Env.mergeValue loc te fe v1 v2)
          | Some v, None | None, Some v -> Some v
          | _ -> None) in
          Env.mergeState loc exp e te fe;
          r

  let iter (b: value -> (value * value) eff) (s: value): value eff =
    raise (SymbolicError (Unknown,"loop"))
  let call (b : unit eff): value eff =
    fun e -> let (r,v) = Env.funScope b e in Some v
  let catch (b: 'a eff) (f: AST.l -> Primops.exc -> 'a eff): 'a eff = 
    fun e -> b e
  let return v: 'a eff = 
    fun e -> 
      Env.return Unknown v e; None
  let throw l x: 'a eff = 
    fun e -> 
      Env.throw l x e; None
  let error l s = 
    fun _ -> raise (SymbolicError (l,s))

end)

let eval_decode_case (loc: AST.l) (env: Env.t) (d: AST.decode_case) (v: value): prog =
  let (_,res) = Env.instScope (Disem.eval_decode_case loc d v) env in
  res

let build_evaluation_environment (defs: AST.declaration list): Env.t =
  try
    let e = Env.empty () in
    let _ = Disem.build_evaluation_environment defs e in
    e
  with
  | SymbolicError (loc, msg) -> 
      Printf.printf "  %s: Evaluation error: %s\n" (pp_loc loc) msg;
      exit 1;

(****************************************************************
 * End
 ****************************************************************)
