(****************************************************************
 * Abstract Interpretation Interface
 ****************************************************************)

module AST = Asl_ast

type fun_sig = (AST.ty option * ((AST.ty * AST.ident) list) * AST.ident list * AST.ident list * AST.l * AST.stmt list)
type inst_sig = AST.encoding * (AST.stmt list) option * bool * AST.stmt list

module type Value = sig
  type value

  (* Value Constructors *)
  val from_bool      : bool -> value
  val from_int       : int -> value
  val from_intLit    : string -> value
  val from_hexLit    : string -> value
  val from_realLit   : string -> value
  val from_bitsLit   : string -> value
  val from_maskLit   : string -> value
  val from_stringLit : string -> value
  val from_enum      : AST.ident -> int -> value
  val from_exc       : AST.l -> Primops.exc -> value
  val from_tuple     : value list -> value

  (* Value Destructors *)
  val to_tuple  : AST.l -> value -> value list
  val to_string : AST.l -> value -> string
  val to_exc    : AST.l -> value -> (AST.l * Primops.exc)

  (* Unit *)
  val vunit   : value
  val is_unit : value -> bool

  (* Bool *)
  val not_bool  : AST.l -> value -> value
  val and_bool  : AST.l -> value -> value -> value
  val eq        : AST.l -> value -> value -> value

  (* Int *)
  val add_int : AST.l -> value -> value -> value
  val sub_int : AST.l -> value -> value -> value
  val leq_int : AST.l -> value -> value -> value

  (* Bitvector *)
  val concat       : AST.l -> value list -> value
  val extract_bits : AST.l -> value -> value -> value -> value
  val width_bits   : AST.l -> value -> value
  val insert_bits  : AST.l -> value -> value -> value -> value -> value
  val inmask       : AST.l -> value -> value -> value

  (* Records *)
  val get_field : AST.l -> value -> AST.ident -> value
  val set_field : AST.l -> value -> AST.ident -> value -> value
  val new_record : (AST.ident * value) list -> value

  (* Array *)
  val get_array : AST.l -> value -> value -> value
  val set_array : AST.l -> value -> value -> value -> value
  val new_array : value -> value

  (* Unknown *)
  val unknown_integer : AST.l -> value
  val unknown_real    : AST.l -> value
  val unknown_string  : AST.l -> value
  val unknown_bits    : AST.l -> value -> value
  val unknown_ram     : AST.l -> value -> value
  val unknown_enum    : AST.l -> value list -> value
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
