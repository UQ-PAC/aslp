(****************************************************************
 * Impl interface
 ****************************************************************)

module type Value = sig
  type 'a eff
  type value
  type fun_sig = (Asl_ast.ty option * ((Asl_ast.ty * Asl_ast.ident) list) * Asl_ast.ident list * Asl_ast.ident list * Asl_ast.l * Asl_ast.stmt list)
  type inst_sig = Asl_ast.encoding * (Asl_ast.stmt list) option * bool * Asl_ast.stmt list

  (* Monadic *)
  val pure : 'a -> 'a eff
  val (>>) : 'a eff -> ('a -> 'b) -> 'b eff
  val (>>=) : 'a eff -> ('a -> 'b eff) -> 'b eff
  val traverse : ('a -> 'b eff) -> 'a list -> 'b list eff

  (* Value Constructors *)
  val from_bool      : bool -> value
  val from_int       : int -> value
  val from_intLit    : string -> value
  val from_hexLit    : string -> value
  val from_realLit   : string -> value
  val from_bitsLit   : string -> value
  val from_maskLit   : string -> value
  val from_stringLit : string -> value
  val from_enum      : Asl_ast.ident -> int -> value

  val uninit_value  : Asl_ast.l -> Asl_ast.ty -> value
  val unknown_value : Asl_ast.l -> Asl_ast.ty -> value

  val to_string : Asl_ast.l -> value -> string
  val to_exc    : Asl_ast.l -> value -> (Asl_ast.l * Primops.exc)
  val from_exc  : Asl_ast.l -> Primops.exc -> value

  (* State *)
  val reset : unit eff

  val nest : 'a eff -> 'a eff
  val nestTop : 'a eff -> 'a eff

  val setVar : Asl_ast.l -> Asl_ast.ident -> value -> unit eff
  val runPrim : string -> value list -> value list -> value option eff

  val isGlobalConstFilter : (Asl_ast.ident -> bool) eff
  val getGlobalConst      : Asl_ast.ident -> value eff
  val getVar              : Asl_ast.l -> Asl_ast.ident -> value eff
  val getImpdef           : Asl_ast.l -> string -> value eff
  val getFun              : Asl_ast.l -> Asl_ast.ident -> fun_sig eff
  val getInstruction      : Asl_ast.l -> Asl_ast.ident -> inst_sig eff
  val getDecoder          : Asl_ast.ident -> Asl_ast.decode_case eff 
  val getEnum             : Asl_ast.ident -> value list option eff

  val addRecord      : Asl_ast.ident -> (Asl_ast.ty * Asl_ast.ident) list -> unit eff
  val addGlobalConst : Asl_ast.ident -> value -> unit eff
  val addGlobalVar   : Asl_ast.ident -> value -> unit eff
  val addEnum        : Asl_ast.ident -> value list -> unit eff
  val addTypedef     : Asl_ast.ident -> Asl_ast.ty -> unit eff
  val addDecoder     : Asl_ast.ident -> Asl_ast.decode_case -> unit eff
  val addInstruction : Asl_ast.l -> Asl_ast.ident -> inst_sig -> unit eff
  val addFun         : Asl_ast.l -> Asl_ast.ident -> fun_sig -> unit eff
  val addLocalVar    : Asl_ast.l -> Asl_ast.ident -> value -> unit eff
  val addLocalConst  : Asl_ast.l -> Asl_ast.ident -> value -> unit eff

  (* Control Flow *)
  val branch    : value -> 'a eff -> 'a eff -> 'a eff
  val iter      : ('a -> ('a * value) eff) -> 'a -> 'a eff
  val return    : value -> 'a eff
  val throw     : Asl_ast.l -> Primops.exc -> 'a eff
  val catch     : unit eff -> (Asl_ast.l -> Primops.exc -> 'a eff) -> 'a eff
  val error     : Asl_ast.l -> string -> 'a eff

  (* Unit *)
  val vunit : value
  val is_unit   : value -> bool

  (* Bool *)
  val not_bool  : Asl_ast.l -> value -> value
  val and_bool  : Asl_ast.l -> value -> value -> value
  val eq        : Asl_ast.l -> value -> value -> value

  (* Int *)
  val add_int : Asl_ast.l -> value -> value -> value
  val sub_int : Asl_ast.l -> value -> value -> value
  val leq_int : Asl_ast.l -> value -> value -> value

  (* Bitvector *)
  val concat       : Asl_ast.l -> value list -> value
  val extract_bits : Asl_ast.l -> value -> value -> value -> value
  val width_bits   : Asl_ast.l -> value -> value
  val insert_bits  : Asl_ast.l -> value -> value -> value -> value -> value
  val inmask       : Asl_ast.l -> value -> value -> value

  (* Tuple *)
  val of_tuple : Asl_ast.l -> value -> value list
  val to_tuple : value list -> value

  (* Records *)
  val get_field : Asl_ast.l -> value -> Asl_ast.ident -> value
  val set_field : Asl_ast.l -> value -> Asl_ast.ident -> value -> value

  (* Array *)
  val get_array : Asl_ast.l -> value -> value -> value
  val set_array : Asl_ast.l -> value -> value -> value -> value

end

module type Semantics = sig
  type 'a eff
  type value

  val eval_expr : Asl_ast.l -> Asl_ast.expr -> value eff
  val eval_stmt : Asl_ast.stmt -> unit eff
  val eval_proccall : Asl_ast.l -> Asl_ast.ident -> value list -> value list -> unit eff
  val eval_funcall : Asl_ast.l -> Asl_ast.ident -> value list -> value list -> value eff
  val eval_decode_case : Asl_ast.l -> Asl_ast.decode_case -> value -> unit eff
  val build_evaluation_environment : Asl_ast.declaration list -> unit eff
end

(****************************************************************
 * End
 ****************************************************************)
