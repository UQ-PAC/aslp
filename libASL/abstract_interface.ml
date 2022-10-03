(****************************************************************
 * Abstract Interpretation Interface
 ****************************************************************)

module AST = Asl_ast

type fun_sig = (AST.ty option * ((AST.ty * AST.ident) list) * AST.ident list * AST.ident list * AST.l * AST.stmt list)
type inst_sig = AST.encoding * (AST.stmt list) option * bool * AST.stmt list

module type Value = sig
  type t

  (* Value Constructors *)
  val from_bool      : bool -> t
  val from_int       : int -> t
  val from_intLit    : string -> t
  val from_hexLit    : string -> t
  val from_realLit   : string -> t
  val from_bitsLit   : string -> t
  val from_maskLit   : string -> t
  val from_stringLit : string -> t
  val from_enum      : AST.ident -> int -> t
  val from_exc       : AST.l -> Primops.exc -> t
  val from_tuple     : t list -> t

  (* Value Destructors *)
  val to_tuple  : AST.l -> t -> t list
  val to_string : AST.l -> t -> string
  val to_exc    : AST.l -> t -> (AST.l * Primops.exc)

  (* Unit *)
  val vunit   : t
  val is_unit : t -> bool

  (* Bool *)
  val not_bool  : AST.l -> t -> t
  val and_bool  : AST.l -> t -> t -> t
  val eq        : AST.l -> t -> t -> t

  (* Int *)
  val add_int : AST.l -> t -> t -> t
  val sub_int : AST.l -> t -> t -> t
  val leq_int : AST.l -> t -> t -> t

  (* Bitvector *)
  val concat_bits  : AST.l -> t list -> t
  val extract_bits : AST.l -> t -> t -> t -> t
  val width_bits   : AST.l -> t -> t
  val insert_bits  : AST.l -> t -> t -> t -> t -> t
  val in_mask      : AST.l -> t -> t -> t

  (* Records *)
  val get_field : AST.l -> t -> AST.ident -> t
  val set_field : AST.l -> t -> AST.ident -> t -> t
  val new_record : (AST.ident * t) list -> t

  (* Array *)
  val get_array : AST.l -> t -> t -> t
  val set_array : AST.l -> t -> t -> t -> t
  val new_array : t -> t

  (* Unknown *)
  val unknown_integer : AST.l -> t
  val unknown_real    : AST.l -> t
  val unknown_string  : AST.l -> t
  val unknown_bits    : AST.l -> t -> t
  val unknown_ram     : AST.l -> t -> t
  val unknown_enum    : AST.l -> t list -> t
end

module type Effect = sig
  type 'a eff
  type value

  (* Monadic *)
  val pure  : 'a -> 'a eff
  val (>>=) : 'a eff -> ('a -> 'b eff) -> 'b eff
  val (>>) : 'a eff -> ('a -> 'b) -> 'b eff
  val traverse : ('a -> 'b eff) -> 'a list -> 'b list eff

  (* State *)
  val reset : unit eff
  val scope : 'a eff -> 'a eff
  val call  : unit eff -> value eff

  val setVar : AST.l -> AST.ident -> value -> unit eff
  val runPrim : string -> value list -> value list -> value option eff
  val isGlobalConstFilter : (AST.ident -> bool) eff

  val getGlobalConst      : AST.ident -> value eff
  val getVar              : AST.l -> AST.ident -> value eff
  val getImpdef           : AST.l -> string -> value eff
  val getFun              : AST.l -> AST.ident -> fun_sig eff
  val getInstruction      : AST.l -> AST.ident -> inst_sig eff
  val getDecoder          : AST.ident -> AST.decode_case eff
  val getEnum             : AST.ident -> value list option eff
  val getRecord           : AST.ident -> (AST.ty * AST.ident) list option eff
  val getTypedef          : AST.ident -> AST.ty option eff

  val addRecord      : AST.ident -> (AST.ty * AST.ident) list -> unit eff
  val addGlobalConst : AST.ident -> value -> unit eff
  val addGlobalVar   : AST.ident -> value -> unit eff
  val addEnum        : AST.ident -> value list -> unit eff
  val addTypedef     : AST.ident -> AST.ty -> unit eff
  val addDecoder     : AST.ident -> AST.decode_case -> unit eff
  val addInstruction : AST.l -> AST.ident -> inst_sig -> unit eff
  val addFun         : AST.l -> AST.ident -> fun_sig -> unit eff
  val addLocalVar    : AST.l -> AST.ident -> value -> unit eff
  val addLocalConst  : AST.l -> AST.ident -> value -> unit eff

  (* Control Flow *)
  val branch    : value -> 'a eff -> 'a eff -> 'a eff
  val iter      : ('a -> ('a * value) eff) -> 'a -> 'a eff
  val return    : value -> 'a eff
  val throw     : AST.l -> Primops.exc -> 'a eff
  val catch     : 'a eff -> (AST.l -> Primops.exc -> 'a eff) -> 'a eff
  val error     : AST.l -> string -> 'a eff
end

(****************************************************************
 * End
 ****************************************************************)
