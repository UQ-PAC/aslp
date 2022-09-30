module PP   = Asl_parser_pp

let pp_exc (exc: Primops.exc): string =
  match exc with
  | Exc_ConstrainedUnpredictable -> "ConstrainedUnpredictable"
  | Exc_ExceptionTaken           -> "ExceptionTaken"
  | Exc_ImpDefined s             -> "ImpDefined" ^ s
  | Exc_SEE s                    -> "SEE" ^ s
  | Exc_Undefined                -> "Undefined"
  | Exc_Unpredictable            -> "Unpredictable"

let pp_unit () = "()"

let pp_list f xs = Printf.sprintf "[%s]" (String.concat " ; " (List.map f xs))

let pp_pair l r (x,y) = Printf.sprintf "(%s, %s)" (l x) (r y)

let type_builtin s = Asl_ast.Type_Constructor (Asl_ast.Ident s)
let type_integer = type_builtin "integer"
let type_bits s = Asl_ast.Type_Bits (Asl_ast.Expr_LitInt s)

let rec nth_modify (f: 'a -> 'a) (n: int) (xs: 'a list): 'a list =
  match n, xs with
  | n, _ when n < 0 -> invalid_arg "nth_modify: negative index"
  | _, [] -> raise Not_found
  | 0, x::rest -> f x :: rest
  | n, x::rest -> x :: nth_modify f (n-1) rest


let pp_decode_pattern (x: Asl_ast.decode_pattern) = Utils.to_string (PP.pp_decode_pattern x)

let pp_decode_slice (x: Asl_ast.decode_slice) = Utils.to_string (PP.pp_decode_slice x)

let pp_decode_alt (DecoderAlt_Alt(ps, _): Asl_ast.decode_alt) = "when (" ^ String.concat ", " (List.map pp_decode_pattern ps) ^ ")"
let pp_decode_case (DecoderCase_Case(slices,_,_): Asl_ast.decode_case) = "case (" ^ String.concat ", " (List.map pp_decode_slice slices) ^ ")"

let pp_instr_field (IField_Field(name,_,_): Asl_ast.instr_field) = Asl_ast.pprint_ident name