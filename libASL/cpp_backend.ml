open Asl_ast
open Asl_utils

(****************************************************************
 * Write State
 ****************************************************************)

type st = {
  mutable depth : int;
  mutable skip_seq : bool;
  oc : out_channel;
  mutable ref_vars : IdentSet.t;

  (* generated instruction functions. *)
  (* XXX: currently only set for decoder function, i.e. assuming only decoder calls other generated functions. *)
  genfns : string list;

  (* variables declared within semantics, i.e. non-global variables *)
  mutable genvars : ident list;

  (* number of identifiers declared in each scope. *)
  mutable genvardepth : int list;
}

let inc_vars st =
  (* output_string st.oc " { "; *)
  st.genvardepth <- (List.hd st.genvardepth + 1) :: List.tl st.genvardepth

let inc_depth st =
  st.depth <- st.depth + 2;
  st.genvardepth <- 0 :: st.genvardepth

let dec_depth st =
  st.depth <- st.depth - 2;
  match st.genvardepth with
  | h::rest ->
      st.genvardepth <- rest;
      (* for _ = 1 to h do output_string st.oc " } " done *)
  | _ -> failwith "genvardepth empty"

let is_ref_var v st =
  IdentSet.mem v st.ref_vars

let clear_ref_vars st =
  st.ref_vars <- IdentSet.empty

let add_ref_var v st =
  st.ref_vars <- IdentSet.add v st.ref_vars

(****************************************************************
 * String Utils
 ****************************************************************)

let gen_regex = Str.regexp {|^\(f_gen_\|f_decl_\)|}
let replace s =
  let s =
    String.fold_left (fun acc c ->
      if c = '.' then acc ^ "_"
      else if c = '#' then acc ^ "HASH"
      else acc ^ (String.make 1 c)) "" s in
  s

let name_of_ident v =
  let s = (match v with
  | Ident n -> "v_" ^ n
  | FIdent (n,0) -> "f_" ^ n
  | FIdent (n,i) -> "f_" ^ n ^ "_" ^ (string_of_int i)) in
  replace s

let prefixed_name_of_ident st v =
  let name = name_of_ident v in
  match v with
  (* non-generated functions and variables are translated to methods on an interface object. *)
  | FIdent (f,_) when not (List.mem f st.genfns) -> "iface." ^ name
  | Ident _ when not (List.mem v st.genvars) -> "iface." ^ name ^ "()"
  | _ -> name

let rec name_of_lexpr l =
  match l with
  | LExpr_Var v -> name_of_ident v
  | LExpr_Field (l, f) ->
      let l = name_of_lexpr l in
      let f = name_of_ident f in
      l ^ "." ^ f
  | LExpr_Wildcard -> "_"
  | _ -> failwith @@ "name_of_lexpr: " ^ (pp_lexpr l)

(****************************************************************
 * File IO
 ****************************************************************)

let write_preamble opens ?(header = true) ?(exports = []) st =
  Printf.fprintf st.oc "/* AUTO-GENERATED LIFTER FILE */\n\n";
  if header then Printf.fprintf st.oc "#pragma once\n";
  List.iter (fun s ->
    Printf.fprintf st.oc "#include \"%s\"\n" s) opens;
  List.iter (fun s ->
    Printf.fprintf st.oc "#include \"%s\" // IWYU pragma: export\n" s) exports;
  Printf.fprintf st.oc "\n";
  Printf.fprintf st.oc "namespace aslp {\n\n"

let write_epilogue fid st =
  Printf.fprintf st.oc "\n} // namespace aslp"

let write_line s st =
  let padding = String.concat "" (List.init st.depth (fun _ -> " ")) in
  output_string st.oc padding;
  output_string st.oc s

let write_seq st =
  if st.skip_seq then
    st.skip_seq <- false
  else Printf.fprintf st.oc ";\n"

let write_nl st =
  Printf.fprintf st.oc "\n"

(****************************************************************
 * Expr Printing
 ****************************************************************)

let rec prints_expr e st =
  match e with
  (* Boolean Expressions *)
  | Expr_Var(Ident "TRUE") -> "true"
  | Expr_Var(Ident "FALSE") -> "false"
  | Expr_TApply(FIdent("and_bool", 0), [], [a;b]) ->
      Printf.sprintf "(%s) && (%s)" (prints_expr a st) (prints_expr b st)
  | Expr_TApply(FIdent("or_bool", 0), [], [a;b]) ->
      Printf.sprintf "(%s) || (%s)" (prints_expr a st) (prints_expr b st)
  | Expr_TApply(FIdent("implies_bool", 0), [], [a;b]) ->
      Printf.sprintf "(%s) ? (%s) : true" (prints_expr a st) (prints_expr b st)
  | Expr_TApply(FIdent("not_bool", 0), [], [a]) ->
      "! (" ^ prints_expr a st ^ ")"

  (* State Accesses *)
  | Expr_Var(v) ->
      let n = prefixed_name_of_ident st v in
      if is_ref_var v st then "" ^ n else n
  | Expr_Field(e, f) ->
      prints_expr e st ^ "." ^ name_of_ident f
  | Expr_Array(a,i) ->
      Printf.sprintf "List.nth (%s) (%s)" (prints_expr a st) (prints_expr i st)

  (* Int Expressions *)
  | Expr_LitInt i ->
      Printf.sprintf "iface.bigint_lit(\"%s\")" i
  | Expr_TApply(FIdent("add_int", 0), [], [a;b]) ->
      Printf.sprintf "(%s) + (%s)" (prints_expr a st) (prints_expr b st)
  | Expr_TApply(FIdent("sub_int", 0), [], [a;b]) ->
      Printf.sprintf "(%s) - (%s)" (prints_expr a st) (prints_expr b st)
  | Expr_TApply(FIdent("mul_int", 0), [], [a;b]) ->
      Printf.sprintf "(%s) * (%s)" (prints_expr a st) (prints_expr b st)
  | Expr_TApply(FIdent("frem_int", 0), [], [a;b]) ->
      Printf.sprintf "(%s) %% (%s)" (prints_expr a st) (prints_expr b st)

  (* Other operations *)
  | Expr_LitBits b ->
      let len = String.length b in
      Printf.sprintf "iface.bits_lit(%dU, \"%s\")" len b
  | Expr_Slices(e,[Slice_LoWd(i,w)]) ->
      let e = prints_expr e st in
      let i = prints_expr i st in
      let w = prints_expr w st in
      Printf.sprintf "iface.extract_bits(%s, /*lo*/ %s, /*wd*/ %s)" e i w
  | Expr_TApply(f, targs, args) ->
      let f = prefixed_name_of_ident st f in
      (* let args = List.map (fun e -> prints_expr e st) (targs @ args) in *)
      let args = List.map (fun e -> prints_expr e st) ([] @ args) in
      f ^ "(" ^ (String.concat ", " args) ^ ")"

  | Expr_LitString s -> "\"" ^ s ^ "\""
  | Expr_Tuple(es) -> "std::make_tuple(" ^ (String.concat "," (List.map (fun e -> prints_expr e st) es)) ^ ")"
  | Expr_Unknown(ty) -> default_value ty st

  | _ -> failwith @@ "prints_expr: " ^ pp_expr e

and default_value t st =
  match t with
  | Type_Bits w ->
      Printf.sprintf "iface.bits_zero(%s)" (prints_expr w st)
  | Type_Constructor (Ident "boolean") -> "true"
  | Type_Constructor (Ident "integer") -> "iface.bigint_zero()"
  | Type_Constructor (Ident "rt_label") -> "typename Traits::rt_label{}"
  | Type_Constructor (Ident "rt_expr") -> "typename Traits::rt_expr{}"
  | Type_Array(Index_Range(lo, hi),ty) ->
      let lo = prints_expr lo st in
      let hi = prints_expr hi st in
      let d = default_value ty st in
      Printf.sprintf "std::vector{(%s)-(%s), %s}" hi lo d
  | _ -> failwith @@ "Unknown type for default value: " ^ (pp_type t)

let prints_ret_type t =
  match t with
  | Some (Type_Constructor (Ident "boolean")) -> "bool"
  | None -> "void"
  | Some t -> failwith @@ "Unknown return type: " ^ (pp_type t)

(****************************************************************
 * Prim Printing
 ****************************************************************)

let write_fun_return e st =
  let s = Printf.sprintf "return (%s)" e in
  write_line s st

let write_proc_return st =
  write_line "return" st

let write_assert s st =
  let s = Printf.sprintf "assert(%s)" s in
  write_line s st

let write_unsupported st =
  write_line {|throw std::runtime_error{"aslp_lifter: unsupported! " + std::string{__func__} + " @ " + std::string{__FILE__} + ":" + std::to_string(__LINE__)}|} st

let write_call f targs args st =
  let f = prefixed_name_of_ident st f in
  let args = [] @ args in
  let call = f ^ "(" ^ (String.concat ", " args) ^ ")" in
  write_line call st

let write_ref v e st =
  let name = prefixed_name_of_ident st v in
  let s = Printf.sprintf "auto %s = %s" name e in
  inc_vars st;
  write_line s st;
  add_ref_var v st

let write_let v e st =
  let v = prefixed_name_of_ident st v in
  let s = Printf.sprintf "const auto %s = %s" v e in
  inc_vars st;
  write_line s st

let write_if_start c st =
  let s = Printf.sprintf "if (%s) {\n" c in
  write_line s st

let write_if_elsif c st =
  let s = Printf.sprintf "} else if (%s) {\n" c in
  write_line s st

let write_if_else st =
  write_line "} else {\n" st

let write_if_end st =
  write_line "} // if\n" st;
  st.skip_seq <- true

(****************************************************************
 * Stmt Printing
 ****************************************************************)

let rec write_lexpr v st =
  match v with
  | LExpr_Wildcard ->
      "std::ignore"

  | LExpr_Var v ->
      name_of_ident v

  | LExpr_Array (LExpr_Var v, i) ->
      let i = prints_expr i st in
      let v = name_of_ident v in
      Printf.sprintf "%s.at(%s)" v i

  | LExpr_Field (l, f) ->
      let v = name_of_lexpr l in
      Printf.sprintf "%s" v

  | LExpr_Tuple (ls) ->
      let vars = List.map (fun l -> write_lexpr l st) ls in
      let v = String.concat "," vars in
      Printf.sprintf "std::tie(%s)" v

  | _ -> failwith @@ "write_assign: " ^ (pp_lexpr v)

let rec write_stmt s st =
  match s with
  | Stmt_VarDeclsNoInit(ty, vs, loc) ->
      let e = default_value ty st in
      st.genvars <- vs @ st.genvars;
      List.iter (fun v -> write_ref v e st) vs

  | Stmt_VarDecl(ty, v, e, loc) ->
      let e = prints_expr e st in
      st.genvars <- v :: st.genvars;
      write_ref v e st

  | Stmt_ConstDecl(ty, v, e, loc) ->
      let e = prints_expr e st in
      st.genvars <- v :: st.genvars;
      write_let v e st

  | Stmt_Assign(l, r, loc) ->
      let e = prints_expr r st in
      let l = write_lexpr l st in
      write_line (Printf.sprintf "%s = %s" l e) st

  | Stmt_TCall(f, tes, es, loc) ->
      let tes = List.map (fun e -> prints_expr e st) tes in
      let es = List.map (fun e -> prints_expr e st) es in
      write_call f tes es st

  | Stmt_FunReturn(e, loc) ->
      write_fun_return (prints_expr e st) st

  | Stmt_ProcReturn(loc) ->
      write_proc_return st

  | Stmt_Assert(e, loc) ->
      write_assert (prints_expr e st) st

  | Stmt_Throw _ ->
      write_unsupported st

  | Stmt_If(c, t, els, f, loc) ->
      let rec iter = function
      | S_Elsif_Cond(c,b)::xs ->
          write_if_elsif (prints_expr c st) st;
          write_stmts b st;
          iter xs
      | [] -> () in
      write_if_start (prints_expr c st) st;
      write_stmts t st;
      iter els;
      if f <> [] then (write_if_else st; write_stmts f st);
      write_if_end st

  | _ -> failwith @@ "write_stmt: " ^ (pp_stmt s);

and write_stmts s st =
  inc_depth st;
  match s with
  | [] ->
      write_proc_return st;
      write_seq st;
      dec_depth st
  | x::xs ->
      write_stmt x st;
      write_seq st;
      List.iter (fun s ->
        write_stmt s st;
        write_seq st;
      ) xs;
      dec_depth st;
      assert (not st.skip_seq)

let build_args prefix targs args =
  if List.length targs = 0 && List.length args = 0 then "()"
  else String.concat " "
    (List.map
      (fun s -> prefix ^ "bits " ^ name_of_ident s)
      (targs@args))

let typenames = ["bits"; "bigint"; "rt_expr"; "rt_lexpr"; "rt_label"]
(* let typenames_upper = List.map String.uppercase_ascii typenames *)
let template_header = "template <lifter_traits Traits>\n"
let prefixed_template_args prefix = "<" ^ prefix ^ "Traits>"
let template_args = prefixed_template_args ""

let write_fn name (ret_tyo,_,targs,args,_,body) st =
  clear_ref_vars st;
  let classname = "aslp_lifter" ^ template_args ^ "::" in
  let args = build_args "Traits::" targs args in
  let ret = prints_ret_type ret_tyo in

  write_line template_header st;
  Printf.fprintf st.oc "%s %s%s(%s) {\n" ret classname (name_of_ident name) args;
  write_stmts body st;
  Printf.fprintf st.oc "\n} // %s\n\n" (name_of_ident name)

(****************************************************************
 * Directory Setup
 ****************************************************************)

let init_st (fnsigs: Eval.fun_sig list) oc =
  let args = List.concat_map
    (fun fnsig -> fnsig_get_args fnsig @ fnsig_get_targs fnsig) fnsigs in
  { depth = 0; skip_seq = false; oc ; ref_vars = IdentSet.empty ;
    genfns = []; genvars = args; genvardepth = []; } 

let stdlib_deps = ["cassert"; "tuple"; "variant"; "vector"; "stdexcept"; "interface.hpp"]
let global_deps = stdlib_deps @ ["aslp_lifter.hpp"; "decode_tests.hpp"]

(* Write an instruction file, containing just the behaviour of one instructions *)
let write_instr_file fn fnsig dir =
  let m = name_of_FIdent fn in
  let path = dir ^ "/" ^ m ^ ".hpp" in
  let oc = open_out path in
  let st = init_st [fnsig] oc in
  write_preamble global_deps st;
  write_fn fn fnsig st;
  write_epilogue () st;
  close_out oc;
  name_of_FIdent fn

(* Write the test file, containing all decode tests *)
let write_test_file tests dir =
  let m = "decode_tests" in
  let path = dir ^ "/" ^ m ^ ".hpp" in
  let oc = open_out path in
  let fnsigs = List.map snd (Bindings.bindings tests) in
  let st = init_st fnsigs oc in
  write_preamble global_deps st;
  Bindings.iter (fun i s -> write_fn i s st) tests;
  write_epilogue () st;
  close_out oc;

  let names = List.map name_of_FIdent @@ List.map fst @@ Bindings.bindings tests in
  (m, names)

(* Write the decoder file - should depend on all of the above *)
let write_decoder_file fn fnsig deps otherfns dir =
  let m = "aslp_lifter_impl" in
  let path = dir ^ "/" ^ m ^ ".hpp" in
  let oc = open_out path in
  let st = init_st [fnsig] oc in
  let st = { st with genfns = otherfns } in
  let deps' = List.map (fun x -> x^".hpp") deps in
  write_preamble global_deps ~exports:deps' st;
  write_fn fn fnsig st;
  write_epilogue fn st;
  close_out oc;
  m 

(** tuple of return type, function name, function arguments (parenthesised) *)
type cpp_fun_sig = (string * string * string)

(* Write the public-facing header file. For compilation speed, this declares but does not define. *)
let write_header_file fn fnsig deps tests dir =
  let name = name_of_FIdent fn in
  let path = dir ^ "/" ^ name ^ ".hpp" in
  let oc = open_out path in
  let st = init_st [fnsig] oc in
  write_preamble stdlib_deps st;

  let void_str = prints_ret_type None in
  write_line template_header st;
  write_line "class aslp_lifter {\n" st;

  inc_depth st;
  write_line ("public: using interface = lifter_interface" ^ template_args ^ ";\n") st;
  write_line "private: interface& iface;\n" st;
  write_line "public:\n" st;
  write_line "aslp_lifter(interface& iface) : iface{iface} { }\n" st;

  let semfns : cpp_fun_sig list = List.map
    (fun f -> (void_str, name_of_ident f, "(Traits::bits)"))
    (fn :: List.map (fun x -> FIdent(x,0)) deps) in

  let testfns : cpp_fun_sig list = List.map
    (fun (k,fnsig) -> (prints_ret_type (fnsig_get_rt fnsig), name_of_ident k, "(Traits::bits)"))
    (Bindings.bindings tests) in

  write_line "/* generated semantics */\n" st;
  List.iter
    (fun (ty,fn,args) -> write_line (ty ^ " " ^ fn ^ args ^ ";\n") st)
    semfns;
  write_line "/* generated decode test conditions */\n" st;
  List.iter
    (fun (ty,fn,args) -> write_line (ty ^ " " ^ fn ^ args ^ ";\n") st)
    testfns;

  dec_depth st;
  write_line "};\n" st;

  write_epilogue fn st;
  close_out oc;
  (name, semfns @ testfns)

(* Creates a directory of explicit instantiations, supporting parallel compilation. *)
let write_explicit_instantiations cppfuns dir =
  (* XXX HACK! we need to properly record which functions are in which file. *)
  let header_file fnfile =
    if String.ends_with ~suffix:"_decode_test" fnfile then
      "gen/decode_tests.hpp"
    else
      "gen/"^fnfile^".hpp" in

  let write_instantiation ((rty, fn, fnargs) : cpp_fun_sig) =
    let fnfile = String.(sub fn 2 (length fn - 2)) in
    let path = dir ^ "/" ^ fnfile ^ ".cpp" in
    let oc = open_out path in
    let st = init_st [] oc in

    write_preamble ~header:false (stdlib_deps @ [header_file fnfile]) st;

    write_line "#ifdef ASLP_LIFTER_INSTANTIATE\n" st;
    write_line "using Traits = ASLP_LIFTER_INSTANTIATE;\n" st;
    let s = Printf.sprintf "template %s %s%s::%s%s;\n" rty "aslp_lifter" template_args fn fnargs in
    write_line s st;
    write_line "#endif\n" st;

    write_epilogue fn st;
    close_out oc;
    fnfile
  in
  List.map write_instantiation cppfuns


(* Write all of the above, expecting Utils.ml to already be present in dir *)
let run dfn dfnsig tests fns rootdir =

  let dir = rootdir ^ "/gen" in
  let instdir = rootdir ^ "/gen-instantiate" in

  if not (Sys.file_exists dir) then Sys.mkdir dir 777;
  if not (Sys.file_exists instdir) then Sys.mkdir instdir 777;
  let files = Bindings.fold (fun fn fnsig acc -> (write_instr_file fn fnsig dir)::acc) fns [] in
  let test_file,tfns = write_test_file tests dir in
  let _decoder = write_decoder_file dfn dfnsig files (files@tfns) dir in
  let (_header, cppfuns) = write_header_file dfn dfnsig files tests dir in
  let _explicits = write_explicit_instantiations cppfuns instdir in
  (* write_dune_file (decoder::files@global_deps) dir *)
  ()
