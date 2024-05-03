
open Asl_ast

let rt_var_ty   = Type_Constructor (Ident "rt_sym")
let rt_label_ty = Type_Constructor (Ident "rt_label")
let rt_expr_ty = Type_Constructor (Ident "rt_expr")

let rt_decl_bv          = FIdent("decl_bv", 0)         (* string -> int -> sym *)
let rt_decl_bool        = FIdent("decl_bool", 0)       (* string -> sym *)
let rt_gen_bit_lit      = FIdent("gen_bit_lit", 0)     (* bv -> bv rt *)
let rt_gen_bool_lit     = FIdent("gen_bool_lit", 0)    (* bool -> bool rt *)
let rt_gen_int_lit      = FIdent("gen_int_lit", 0)     (* int -> int rt *)
let rt_gen_slice        = FIdent("gen_slice", 0)       (* bv rt -> int -> int -> bv rt *)

let rt_gen_branch       = FIdent("gen_branch", 0)      (* bool rt -> (rt_label, rt_label, rt_label) *)
let rt_true_branch = FIdent("true_branch", 0)
let rt_false_branch = FIdent("false_branch", 0)
let rt_merge_branch = FIdent("merge_branch", 0)

let rt_switch_context   = FIdent("switch_context", 0)  (* rt_label -> unit *)
let rt_gen_load         = FIdent("gen_load", 0)        (* sym -> 'a rt *)
let rt_gen_store        = FIdent("gen_store", 0)       (* sym -> 'a rt -> unit *)
let rt_gen_assert       = FIdent("gen_assert", 0)      (* bool rt -> unit *)

let rt_gen_array_store  = FIdent("gen_array_store", 0) (* sym -> int -> 'a rt -> unit *)
let rt_gen_array_load   = FIdent("gen_array_load", 0)  (* sym -> int -> 'a rt *)
