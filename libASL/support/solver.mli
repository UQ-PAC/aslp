(****************************************************************)
(** {3 Z3 support code}                                         *)
(****************************************************************)

(** check that bs => cs *)
val check_constraints : (Asl_ast.expr list) -> (Asl_ast.expr list) -> bool
