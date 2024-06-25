(****************************************************************)
(** {3 Z3 support code}                                         *)
(****************************************************************)

(** check that bs => cs *)
val check_constraints : (LibASL_base.Asl_ast.expr list) -> (LibASL_base.Asl_ast.expr list) -> bool
