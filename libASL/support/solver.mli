(****************************************************************)
(** {3 Z3 support code}                                         *)
(****************************************************************)

open LibASL_stage0

(** check that bs => cs *)
val check_constraints : (Asl_ast.expr list) -> (Asl_ast.expr list) -> bool
