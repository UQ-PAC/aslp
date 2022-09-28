module AST  = Asl_ast
module TC   = Tcheck

open AST
open Value
open Utils
open Asl_utils


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

module ImpDefs = struct
    include Map.Make(struct
        type t = string
        let compare = String.compare
    end)
end
  
type scope = { mutable bs : value Bindings.t; }

let empty_scope (_: unit): scope =
    let bs = Bindings.empty in
    { bs }

let mem_scope (k: ident) (s: scope): bool =
    Bindings.mem k s.bs

let get_scope (k: ident) (s: scope): value =
    Bindings.find k s.bs

let set_scope (k: ident) (v: value) (s: scope): unit =
    s.bs <- Bindings.add k v s.bs

module Env : sig
  type t

  val empty               : t
  val nestTop             : (t -> 'a) -> (t -> 'a)
  val nest                : (t -> 'a) -> (t -> 'a)

  val addLocalVar         : Asl_ast.l -> t -> Asl_ast.ident -> value -> unit
  val addLocalConst       : Asl_ast.l -> t -> Asl_ast.ident -> value -> unit

  val addGlobalConst      : t -> Asl_ast.ident -> value -> unit
  val getGlobalConst      : t -> Asl_ast.ident -> value

  (* to support generation of unknown values, we need to remember the structure
   * of user-defined types such as enumerations and records
   *)
  val addEnum             : t -> Asl_ast.ident -> value list -> unit
  val getEnum             : t -> Asl_ast.ident -> (value list) option
  val isEnumEq            : t -> Asl_ast.ident -> bool
  val isEnumNeq           : t -> Asl_ast.ident -> bool

  val addRecord           : Asl_ast.ident -> (Asl_ast.ty * Asl_ast.ident) list -> t -> unit
  val getRecord           : Asl_ast.ident -> t -> (Asl_ast.ty * Asl_ast.ident) list option

  val addTypedef          : t -> Asl_ast.ident -> Asl_ast.ty -> unit
  val getTypedef          : t -> Asl_ast.ident -> Asl_ast.ty option

  val addGlobalVar        : t -> Asl_ast.ident -> value -> unit
  val getVar              : Asl_ast.l -> t -> Asl_ast.ident -> value
  val setVar              : Asl_ast.l -> t -> Asl_ast.ident -> value -> unit

  val getFun              : Asl_ast.l -> t -> Asl_ast.ident -> (Asl_ast.ident list * Asl_ast.ident list * Asl_ast.l * Asl_ast.stmt list)
  val addFun              : Asl_ast.l -> t -> Asl_ast.ident -> (Asl_ast.ident list * Asl_ast.ident list * Asl_ast.l * Asl_ast.stmt list) -> unit

  val getInstruction      : Asl_ast.l -> t -> Asl_ast.ident -> (Asl_ast.encoding * (Asl_ast.stmt list) option * bool * Asl_ast.stmt list)
  val addInstruction      : Asl_ast.l -> t -> Asl_ast.ident -> (Asl_ast.encoding * (Asl_ast.stmt list) option * bool * Asl_ast.stmt list) -> unit

  val getDecoder          : t -> Asl_ast.ident -> Asl_ast.decode_case
  val addDecoder          : t -> Asl_ast.ident -> Asl_ast.decode_case -> unit

  val setImpdef           : t -> string -> value -> unit
  val getImpdef           : Asl_ast.l -> t -> string -> value
end = struct
  type t = {
    mutable instructions : (Asl_ast.encoding * (Asl_ast.stmt list) option * bool * Asl_ast.stmt list) Bindings.t;
    mutable decoders     : Asl_ast.decode_case Bindings.t;
    mutable functions    : (Asl_ast.ident list * Asl_ast.ident list * Asl_ast.l * Asl_ast.stmt list) Bindings.t;
    mutable enums        : (value list) Bindings.t;
    mutable enumEqs      : IdentSet.t;
    mutable enumNeqs     : IdentSet.t;
    mutable records      : ((Asl_ast.ty * Asl_ast.ident) list) Bindings.t;
    mutable typedefs     : Asl_ast.ty Bindings.t;
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

  let addLocalVar (loc: Asl_ast.l) (env: t) (x: Asl_ast.ident) (v: value): unit =
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

  let addRecord (x: ident) (fs: (AST.ty * ident) list) (env: t): unit =
      env.records <- Bindings.add x fs env.records

  let getRecord (x: ident) (env: t): ((AST.ty * ident) list) option =
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

  let getFun (loc: l) (env: t) (x: ident): (ident list * ident list * AST.l * stmt list) =
      (match Bindings.find_opt x env.functions with
      | Some def -> def
      | None     -> raise (EvalError (loc, "getFun " ^ pprint_ident x))
      )

  let addFun (loc: l) (env: t) (x: ident) (def: (ident list * ident list * AST.l * stmt list)): unit =
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

  let addDecoder (env: t) (x: Asl_ast.ident) (d: Asl_ast.decode_case): unit =
      env.decoders <- Bindings.add x d env.decoders

  let setImpdef (env: t) (x: string) (v: value): unit =
      env.impdefs <- ImpDefs.add x v env.impdefs

  let getImpdef (loc: Asl_ast.l) (env: t) (x: string): value =
      (match ImpDefs.find_opt x env.impdefs with
      | Some v -> v
      | None ->
              raise (EvalError (loc, "Unknown value for IMPLEMENTATION_DEFINED \""^x^"\""))
      )
end

module Real : Impl.Value = struct
  type value = Value.value
  type 'a eff = Env.t -> 'a
  type fun_sig = (Asl_ast.ty option * ((Asl_ast.ty * Asl_ast.ident) list) * Asl_ast.ident list * Asl_ast.ident list * Asl_ast.l * Asl_ast.stmt list)
  type inst_sig = Asl_ast.encoding * (Asl_ast.stmt list) option * bool * Asl_ast.stmt list

  (* Monadic *)
  let pure a e = a
  let (>>) a f e = let (r) = a e in (f r)
  let (>>=) a f e = let (r) = a e in f r e
  let rec traverse f xs e =
    match xs with
    | [] -> ([])
    | (x::xs) -> 
        let (x') = f x e in
        let (xs') = traverse f xs e in
        (x'::xs')

  (* Value Constructors *)
  let from_bool b = Value.VBool b
  let from_int i = Value.VInt (Z.of_int i)
  let from_intLit = Value.from_intLit
  let from_hexLit = Value.from_hexLit
  let from_realLit = Value.from_realLit
  let from_bitsLit = Value.from_bitsLit
  let from_maskLit = Value.from_maskLit
  let from_stringLit = Value.from_stringLit
  let from_enum e i = Value.VEnum (e, i)

  let uninit_value loc x = Value.VUninitialized
  let unknown_value loc x = Value.VUninitialized

  let to_string loc v = Value.to_string loc v
  let to_exc loc v = Value.to_exc loc v
  let from_exc loc e = Value.VExc (loc, e)

  (* State *)
  let reset e = ()
  let nest = Env.nest
  let nestTop = Env.nestTop

  let setVar = Env.setVar
  let runPrim = Value.eval_prim

  let isGlobalConstFilter = Env.getGlobalConst 
  let getGlobalConst      = Env.getGlobalConst 
  let getVar              = Env.getVar
  let getImpdef           = Env.getImpdef
  let getFun              = Env.getFun
  let getInstruction      = Env.getInstruction
  let getDecoder          = Env.getDecoder
  let getEnum             = Env.getEnum

  let addRecord = Env.addRecord
  let addGlobalConst = Env.addGlobalConst
  let addGlobalVar = Env.addGlobalVar
  let addEnum = Env.addEnum
  let addTypedef = Env.addTypedef
  let addDecoder = Env.addDecoder
  let addInstruction = Env.addInstruction
  let addFun = Env.addFun
  let addLocalVar = Env.addLocalVar
  let addLocalConst = Env.addLocalConst

  (* Control Flow *)
  let branch l c t f = if Value.to_bool l ( c() ) then t () else f ()
  let rec iter l b s =
    let (s',c) = b s () in
    if Value.to_bool l c then iter l b s' else s'
  let return v = raise (Value.Return (Some v))
  let throw l e = raise (Value.Throw (l,e))
  let catch b f = try b () with Value.Throw (l,e) -> f l e ()
  let error l s = raise (Value.EvalError (l,s))

  (* Unit *)
  let vunit = Value.VTuple []
  let is_unit v = match v with Value.VTuple [] -> true | _ -> false

  (* Bool *)
  let not_bool l v = Value.VBool (not (Value.to_bool l v))
  let and_bool l v1 v2 = Value.VBool (Value.to_bool l v1 && Value.to_bool l v2)
  let eq = Value.eval_eq

  (* Int *)
  let add_int = Value.eval_add_int
  let sub_int = Value.eval_sub_int
  let leq_int = Value.eval_leq

  (* Bitvector *)
  let concat = Value.eval_concat
  let extract_bits = Value.extract_bits
  let width_bits l v = Primops.prim_length_bits (Value.to_bits l v)
  let insert_bits = Value.insert_bits
  let inmask = Value.eval_inmask

  (* Tuple *)
  let of_tuple = Value.of_tuple
  let to_tuple = Value.to_tuple

  (* Records *)
  let get_field = Value.get_field
  let set_field = Value.set_field

  (* Array *)
  let get_array = Value.get_array
  let set_array = Value.set_array

end
