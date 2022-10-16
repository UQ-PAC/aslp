(****************************************************************
 * ASL evaluator
 *
 * Copyright Arm Limited (c) 2017-2019
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL evaluator *)

module PP   = Asl_parser_pp
module AST  = Asl_ast
module TC   = Tcheck

open AST
open Asl_utils
open Value
open Abstract_interface

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

let get_scope_opt (k: ident) (s: scope): value option =
    Bindings.find_opt k s.bs

let set_scope (k: ident) (v: value) (s: scope): unit =
    s.bs <- Bindings.add k v s.bs


(****************************************************************)
(** {2 Mutable bindings}                                        *)
(****************************************************************)

(** Environment representing both global and local state of the system *)
module Env : sig
    type t

    val empty               : t
    val nestTop             : (t -> 'a) -> (t -> 'a)
    val nest                : (t -> 'a) -> (t -> 'a)

    val addLocalVar         : AST.l -> t -> ident -> value -> unit
    val addLocalConst       : AST.l -> t -> ident -> value -> unit

    val addGlobalConst      : t -> ident -> value -> unit
    val getGlobalConst      : t -> ident -> value

    (* to support generation of unknown values, we need to remember the structure
     * of user-defined types such as enumerations and records
     *)
    val addEnum             : t -> ident -> value list -> unit
    val getEnum             : t -> ident -> (value list) option
    val isEnumEq            : t -> ident -> bool
    val isEnumNeq           : t -> ident -> bool

    val addRecord           : t -> ident -> (AST.ty * ident) list -> unit
    val getRecord           : t -> ident -> (AST.ty * ident) list option

    val addTypedef          : t -> ident -> AST.ty -> unit
    val getTypedef          : t -> ident -> AST.ty option

    val addGlobalVar        : t -> ident -> value -> unit
    val getVar              : AST.l -> t -> ident -> value
    val setVar              : AST.l -> t -> ident -> value -> unit

    val getFun              : AST.l -> t -> ident -> fun_sig
    val addFun              : AST.l -> t -> ident -> fun_sig -> unit

    val getInstruction      : AST.l -> t -> ident -> (encoding * (stmt list) option * bool * stmt list)
    val addInstruction      : AST.l -> t -> ident -> (encoding * (stmt list) option * bool * stmt list) -> unit

    val getDecoder          : t -> ident -> decode_case
    val addDecoder          : t -> ident -> decode_case -> unit

    val setImpdef           : t -> string -> value -> unit
    val getImpdef           : AST.l -> t -> string -> value

end = struct
    type t = {
        mutable instructions : (encoding * (stmt list) option * bool * stmt list) Bindings.t;
        mutable decoders     : decode_case Bindings.t;
        mutable functions    : fun_sig Bindings.t;
        mutable enums        : (value list) Bindings.t;
        mutable enumEqs      : IdentSet.t;
        mutable enumNeqs     : IdentSet.t;
        mutable records      : ((AST.ty * ident) list) Bindings.t;
        mutable typedefs     : AST.ty Bindings.t;
        mutable globals      : scope;
        mutable constants    : scope;
        mutable impdefs      : value ImpDefs.t;
        mutable locals       : scope list
    }

    let empty = {
        decoders     = Bindings.empty;
        instructions = Bindings.empty;
        functions    = Bindings.empty;
        enums        = Bindings.empty;
        enumEqs      = IdentSet.empty;
        enumNeqs     = IdentSet.empty;
        records      = Bindings.empty;
        typedefs     = Bindings.empty;
        globals      = empty_scope ();
        constants    = empty_scope ();
        impdefs      = ImpDefs.empty;
        locals       = [empty_scope ()];
    }

    let nestTop (k: t -> 'a) (parent: t): 'a =
        let child = {
            decoders     = parent.decoders;
            instructions = parent.instructions;
            functions    = parent.functions;
            enums        = parent.enums;
            enumEqs      = parent.enumEqs;
            enumNeqs     = parent.enumNeqs;
            records      = parent.records;
            typedefs     = parent.typedefs;
            globals      = parent.globals;
            constants    = parent.constants;
            impdefs      = parent.impdefs;
            locals       = [empty_scope ()];  (* only change *)
        } in
        k child

    let nest (k: t -> 'a) (parent: t): 'a =
        let child = {
            decoders     = parent.decoders;
            instructions = parent.instructions;
            functions    = parent.functions;
            enums        = parent.enums;
            enumEqs      = parent.enumEqs;
            enumNeqs     = parent.enumNeqs;
            records      = parent.records;
            typedefs     = parent.typedefs;
            globals      = parent.globals;
            constants    = parent.constants;
            impdefs      = parent.impdefs;
            locals       = empty_scope () :: parent.locals;  (* only change *)
        } in
        k child

    let addLocalVar (loc: l) (env: t) (x: ident) (v: value): unit =
        if !trace_write then Printf.printf "TRACE: fresh %s = %s\n" (pprint_ident x) (pp_value v);
        (match env.locals with
        | (bs :: _) -> set_scope x v bs
        | []        -> raise (EvalError (loc, "addLocalVar"))
        )

    let addLocalConst (loc: l) (env: t) (x: ident) (v: value): unit =
        (* todo: should constants be held separately from local vars? *)
        (match env.locals with
        | (bs :: _) -> set_scope x v bs
        | []        -> raise (EvalError (loc, "addLocalConst"))
        )

    let addGlobalConst (env: t) (x: ident) (v: value): unit =
        set_scope x v env.constants

    let getGlobalConst (env: t) (x: ident): value =
        get_scope x env.constants

    let addEnum (env: t) (x: ident) (vs: value list): unit =
        env.enums    <- Bindings.add x vs env.enums

    let getEnum (env: t) (x: ident): (value list) option =
        Bindings.find_opt x env.enums

    let isEnumEq  (env: t) (x: ident): bool = IdentSet.mem x env.enumEqs
    let isEnumNeq (env: t) (x: ident): bool = IdentSet.mem x env.enumNeqs

    let addRecord (env: t) (x: ident) (fs: (AST.ty * ident) list): unit =
        env.records <- Bindings.add x fs env.records

    let getRecord (env: t) (x: ident): ((AST.ty * ident) list) option =
        Bindings.find_opt x env.records

    let addTypedef (env: t) (x: ident) (ty: AST.ty): unit =
        env.typedefs <- Bindings.add x ty env.typedefs

    let getTypedef (env: t) (x: ident): AST.ty option =
        Bindings.find_opt x env.typedefs

    let addGlobalVar (env: t) (x: ident) (v: value): unit =
        set_scope x v env.globals

    let findScope (env: t) (x: ident): scope option =
        let rec search (bss : scope list): scope option =
            (match bss with
            | (bs :: bss') ->
                    if mem_scope x bs then Some bs else search bss'
            | [] ->
                    if mem_scope x env.globals then Some env.globals
                    else if mem_scope x env.constants then Some env.constants
                    else None
            )
        in
        search env.locals

    let getVar (loc: l) (env: t) (x: ident): value =
        (match findScope env x with
        | Some bs -> get_scope x bs
        | None    -> raise (EvalError (loc, "getVar: " ^ pprint_ident x))
        )

    let setVar (loc: l) (env: t) (x: ident) (v: value): unit =
        if !trace_write then Printf.printf "TRACE: write %s = %s\n" (pprint_ident x) (pp_value v);
        (match findScope env x with
        | Some bs -> set_scope x v bs
        | None    -> raise (EvalError (loc, "setVar " ^ pprint_ident x))
        )

    let getFun (loc: l) (env: t) (x: ident): fun_sig =
        (match Bindings.find_opt x env.functions with
        | Some def -> def
        | None     -> raise (EvalError (loc, "getFun " ^ pprint_ident x))
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
                raise (EvalError (loc, "Unknown value for IMPLEMENTATION_DEFINED \""^x^"\""))
        )
end


(****************************************************************)
(** {2 Abstract Interpretation }                                *)
(****************************************************************)

module Semantics = Abstract.Make(struct
  type t = Value.value

  let pp_value = pp_value

  (* Value Constructors *)
  let mk_bool b     = Value.VBool b
  let mk_int i      = Value.VInt (Z.of_int i)
  let mk_bigint i   = Value.VInt i
  let mk_real q     = Value.VReal q
  let mk_bits n v   = Value.VBits (Primops.mkBits n v)
  let mk_mask n v m = Value.VMask (Primops.mkMask n v m)
  let mk_string s   = Value.VString s

  let mk_enum e i  = Value.VEnum (e, i)
  let mk_exc loc e = Value.VExc (loc, e)
  let mk_tuple     = Value.to_tuple

  (* Value Destructors *)
  let get_string loc v = Value.to_string loc v
  let get_exc loc v    = Value.to_exc loc v
  let get_tuple        = Value.of_tuple

  (* Unit *)
  let unit      = Value.VTuple []
  let is_unit v = match v with Value.VTuple [] -> true | _ -> false

  (* Bool *)
  let not_bool l v     = Value.VBool (not (Value.to_bool l v))
  let and_bool l v1 v2 = Value.VBool (Value.to_bool l v1 && Value.to_bool l v2)
  let eq l v1 v2       = Value.VBool (Value.eval_eq l v1 v2)

  (* Int *)
  let add_int         = Value.eval_add_int
  let sub_int         = Value.eval_sub_int
  let leq_int l v1 v2 = Value.VBool (Value.eval_leq l v1 v2)

  (* Bitvector *)
  let concat_bits     = Value.eval_concat
  let extract_bits    = Value.extract_bits'' (* extract_bits'' allows extracting from ints *)
  let width_bits l v  = Value.VInt (Z.of_int (Primops.prim_length_bits (Value.to_bits l v)))
  let insert_bits     = Value.insert_bits
  let in_mask l v1 v2 = Value.VBool (Value.eval_inmask l v1 v2)

  (* Records *)
  let get_field = Value.get_field
  let set_field = Value.set_field
  let new_record = Value.mkrecord

  (* Array *)
  let get_array = Value.get_array
  let set_array = Value.set_array
  let new_array = Value.empty_array

  (* Unknown *)
  let unknown_integer _ = Value.eval_unknown_integer ()
  let unknown_string _ = Value.eval_unknown_string ()
  let unknown_real _ = Value.eval_unknown_real ()
  let unknown_bits l v = Value.eval_unknown_bits (Value.to_integer l v)
  let unknown_ram l v = Value.eval_unknown_ram (Value.to_integer l v)
  let unknown_enum _ es = match es with e::_ -> e | _ -> VUninitialized

end) (struct
  type value = Value.value
  type 'a eff = Env.t -> 'a

  (* Monadic *)
  let pure a e = a
  let (>>) a f e = let (r) = a e in (f r)
  let (>>=) a f e = let (r) = a e in f r e

  (* State *)
  let reset = pure ()
  let scope = Env.nest
  let call (b : unit eff) e =
    try
      Env.nestTop b e;
      Value.VTuple []
    with
    | Return v -> v
    | Throw (l, ex) -> raise (Throw (l,ex))

  let runPrim f tes es e = Value.eval_prim f tes es
  let isGlobalConstFilter env t =
    match Env.getGlobalConst env t with
    | _ -> true
    | exception _ -> false

  let addLocalVar   (loc: l) (x: ident) (v: value) (env: Env.t) = Env.addLocalVar   loc env x v
  let addLocalConst (loc: l) (x: ident) (v: value) (env: Env.t) = Env.addLocalConst loc env x v

  let addGlobalConst (x: ident) (v: value) (env: Env.t) = Env.addGlobalConst env x v
  let getGlobalConst (x: ident) (env: Env.t) = Env.getGlobalConst env x

  let addEnum (x: ident) (vs: value list) (env: Env.t) = Env.addEnum env x vs
  let getEnum (x: ident) (env: Env.t) = Env.getEnum env x

  let addRecord (x: ident) (vs: (AST.ty * ident) list) (env: Env.t) = Env.addRecord env x vs
  let getRecord (x: ident) (env: Env.t) = Env.getRecord env x

  let addTypedef (x: ident) (t: AST.ty) (env: Env.t) = Env.addTypedef env x t
  let getTypedef (x: ident) (env: Env.t) = Env.getTypedef env x

  let addGlobalVar (x: ident) (v: value) (env: Env.t)    = Env.addGlobalVar env x v
  let getVar (loc: l) (x: ident) (env: Env.t)            = Env.getVar loc env x
  let setVar (loc: l) (x: ident) (v: value) (env: Env.t) = Env.setVar loc env x v

  let getFun (loc: l) (x: ident) (env: Env.t) = Env.getFun loc env x
  let addFun (loc: l) (x: ident) (f: fun_sig) (env: Env.t) = Env.addFun loc env x f

  let getInstruction (loc: l) (x: ident) (env: Env.t) = Env.getInstruction loc env x
  let addInstruction (loc: l) (x: ident) (i: inst_sig) (env: Env.t) = Env.addInstruction loc env x i

  let getDecoder (x: ident) (env: Env.t)                  = Env.getDecoder env x
  let addDecoder (x: ident) (d: decode_case) (env: Env.t) = Env.addDecoder env x d

  let getImpdef (loc: l) (s: string) (env: Env.t) = Env.getImpdef loc env s

  (* Control Flow *)
  let branch c t f e = if Value.to_bool Unknown ( c ) then t e else f e
  let rec iter b s e =
    let (s',c) = b s e in
    if Value.to_bool Unknown c then iter b s' e else s'
  let return v _ = raise (Value.Return v)
  let throw l e _ = raise (Value.Throw (l,e))
  let catch b f e = try b e with Value.Throw (l,x) -> f l x e
  let error l s e = raise (Value.EvalError (l,s))
end)


(****************************************************************)
(** {2 Compatibility Interface}                                 *)
(****************************************************************)

let eval_expr (loc: AST.l) (env: Env.t) (e: AST.expr): value =
  Semantics.eval_expr loc e env

let eval_stmt (env: Env.t) (xs: AST.stmt): unit =
  Semantics.eval_stmt xs env

let eval_proccall (loc: l) (env: Env.t) (f: ident) (tvs: value list) (vs: value list): unit =
  Semantics.eval_proccall loc f tvs vs env

let eval_funcall (loc: l) (env: Env.t) (f: ident) (tvs: value list) (vs: value list): value =
  Semantics.eval_funcall loc f tvs vs env

let eval_decode_case (loc: AST.l) (env: Env.t) (d: AST.decode_case) (v: value): unit =
  Semantics.eval_decode_case loc d v env

let build_evaluation_environment (defs: AST.declaration list): Env.t =
  let e = Env.empty in
  let () = Semantics.build_evaluation_environment defs e in
  e

(****************************************************************
 * End
 ****************************************************************)
