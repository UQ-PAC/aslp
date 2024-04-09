open LibASL
open Testing
open Asl_ast
open Asl_visitor
open Value
open Utils

module IntSet = Set.Make(Int)




module NoCSV  = struct 
  open List 

  type cell = 
    | FloatC of float
    | IntC of int
    | StringC of string

  type row  = cell list 

  type table = {
    header: string list;
    rows:row list 
  }

  let delim = "\t"

  let cell_to_csv (c:cell)  = match c with 
    | FloatC f -> Printf.sprintf "%f" f
    | IntC f -> Printf.sprintf "%d" f
    | StringC f -> Printf.sprintf "%s" f

  let make headers rows = {
    header = headers;
    rows = rows
    }

  let row_to_csv r = String.concat delim (List.map cell_to_csv (r)) 

  let to_csv (t:table) = (String.concat delim t.header) ^ "\n" ^ (String.concat "\n" (List.map row_to_csv t.rows))

  let trans b (x: 'a list list ) : 'a list list = 
    let rec tr acc (rest: 'a list list)  = 
      match rest with 
        | hd::tl -> 
          (match hd with 
            | [] -> tr acc tl
            | hd -> 
              let col = hd in
              let rest = tl in
              let cc = List.map2 (fun x y -> x@[y]) acc col in
              tr cc rest)
        | [] -> acc
    in tr (List.map (fun _ -> []) (List.hd x)) x

  let add_columns (t:table) (cols:cell list list) = 
    {t with rows = (trans t.rows cols)}
end


let dis_online env (iset: string) (op: int): (stmt list*Mtime.span) opresult =
  let c = Mtime_clock.counter () in
  let lenv = Dis.build_env env in
  let decoder = Eval.Env.getDecoder env (Ident iset) in
  let bv = Primops.prim_cvt_int_bits (Z.of_int 32) (Z.of_int op) in


  try
    let stmts = Dis.dis_decode_entry env lenv decoder (VBits bv) in
    let ts = Mtime_clock.count c in
    Result.Ok (stmts, ts)
  with
    | e -> Result.Error (Op_DisFail e)

let dis_offline (op: int): (stmt list * Mtime.span) opresult =
  let c = Mtime_clock.counter () in
  let bv = Primops.prim_cvt_int_bits (Z.of_int 32) (Z.of_int op) in
  try
    let stmts = OfflineASL.Offline.run bv in
    let ts = Mtime_clock.count c in
    Result.Ok (stmts, ts)
  with
    | e -> Result.Error (Op_DisFail e)

let unres r = match r with 
  | Result.Ok stmts -> fst stmts
  | Result.Error _ -> []
    
module StringMap = Map.Make(String) 

type dis_single = (stmt list * Mtime.span) opresult


type dis_res = {
  online:  dis_single;
  offline: dis_single;
}

type resultstype = (string * i * dis_res) list


class branch_counter = object(this)
  inherit Asl_visitor.nopAslVisitor
  val mutable stmt_count: int  = 0
  val mutable expr_count: int  = 0
  val mutable branch_count: int  = 0
  val mutable var_decl_count: int  = 0

  method! vstmt s = 
    stmt_count <- stmt_count + 1 ;
    match s with 
      | Stmt_ConstDecl _ -> var_decl_count <- var_decl_count + 1 ; DoChildren
      | Stmt_VarDecl _ -> var_decl_count <- var_decl_count + 1 ; DoChildren
      | Stmt_VarDeclsNoInit (ty, is, _) -> var_decl_count <- var_decl_count + (List.length is) ; DoChildren
      | Stmt_If (c, t, els, e, loc) ->
        branch_count <- branch_count + 1 ;
        DoChildren
      | _ -> DoChildren


  method! vexpr s = expr_count <- expr_count + 1; DoChildren
  
  method count (s:stmt) : int = stmt_count <- 0; (visit_stmt this s) |> ignore; stmt_count

  method expr_count (e:expr) : int = expr_count <- 0; (visit_expr this e) |> ignore ; expr_count
  method gexpr_count = expr_count
  method gstmt_count = stmt_count 
  method gbr_count = branch_count 
  method g_vars = var_decl_count 
end

let count_stmts_list (s:stmt list) : int = 
  let v = (new branch_counter) in
  visit_stmts v s |> ignore ;
  (v#gexpr_count + v#gstmt_count)
  

let count_branch (s:stmt list) : int = 
  let v = (new branch_counter) in
  visit_stmts v s |> ignore ;
  (v#gbr_count)





let get_int_opcodes verbose instr env : (string * int) list = 
  let iset = "A64" in
  let encodings = get_opcodes verbose iset instr env in
  let results = List.concat_map (fun (enc, fields, opt_opcodes) ->
      match opt_opcodes with
      | None -> []
      | Some opcodes -> List.concat_map (function 
        | (op, true) -> [enc,op] 
        | _ -> []
      ) opcodes 
  ) encodings in
  results


let lift_offline (ops: (string * int) list) : (string * i * dis_single) list = List.map (fun (insn, op) -> insn, op, (dis_offline op)) ops

let rec process_command tcenv env cmd =
  match String.split_on_char ' ' cmd with
  | (":set" :: "impdef" :: rest) ->
        let cmd = String.concat " " rest in
        let (x, e) = LoadASL.read_impdef tcenv Unknown cmd in
        let v = Eval.eval_expr Unknown env e in
        Eval.Env.setImpdef env x v
  | [":project"; prj] ->
      let inchan = open_in prj in
      (try
          while true do
              process_command tcenv env (input_line inchan)
          done
      with
      | End_of_file ->
          close_in inchan
      )
  | [""] -> ()
  | _ -> Printf.printf "Ignoring: %s\n" cmd



let x = ref None


let get_lifter (u:unit) = 
  (* Lazily load asl files *)
  let load_env_get_lifter (u:unit) = 
    let opt_verbose = ref true in
    let c = Mtime_clock.counter () in
    let env = match Eval.aarch64_evaluation_environment ~verbose:!opt_verbose () with
    | Some e -> e
    | _ -> failwith "Unable to build evaluation environment." in
    let filenames = Option.get Eval.aarch64_asl_files in
    let prj_files = List.filter (fun f -> Utils.endswith f ".prj") (snd filenames) in
    let tcenv = Tcheck.Env.mkEnv Tcheck.env0 in
    let delta = Mtime_clock.count c in
    Printf.printf "Startup time %f" (span_to_float_s delta) ;
    List.iter (fun f -> process_command tcenv env (":project " ^ f)) prj_files;
    let lift_online_ops  env (ops: (string * int) list) : (string * i * dis_single) list = List.map (fun (instr, op) -> instr , op, (dis_online env "A64" op)) ops in
    let get_opcode instr = get_int_opcodes opt_verbose instr env in
    let lift_online_ops (ops:(string * int) list) = lift_online_ops env ops in

    let all_insn_opcodes : ((bitsLit * i) list)  = 
      let patterns = [
        "aarch64_integer.+";
        "aarch64_branch.+";
        "aarch64_float_.+";
        "aarch64_vector_arithmetic_binary.+";
        "aarch64_vector_(?!arithmetic_binary).+";
        "aarch64_memory_.+"
      ] in List.concat_map (fun p -> get_opcode p) patterns 
    in

    get_opcode, lift_online_ops, lift_offline, all_insn_opcodes in
  match !x with 
    | Some l -> l
    | None -> x := Some  (load_env_get_lifter ()) ; Option.get !x

let lift_online ops = 
  let _,lift_online_ops,_,_ = get_lifter () in
  lift_online_ops ops

let get_opcode op = 
  let get_opcodes,_,_,_ = get_lifter () in
  get_opcodes op

let all_insn_opcodes (u:unit) = 
  let _,_,_,ops = get_lifter () in ops


let group_list (k: 'b -> string) (l: 'b list) : ('b list) StringMap.t = 
  let keys = StringSet.of_list (List.map k l) in
  let grouped = StringMap.of_seq (Seq.map (fun key -> (key, List.find_all (fun e -> (k e) = key) l)) (StringSet.to_seq keys)) in
  grouped


type grouped_res = ((string * i * dis_single) list) StringMap.t

(*

- count dis passed by insn
- count assembly size by insn
- summarise time by insn 

*)

let count_success_dis (i:grouped_res) : 'a StringMap.t = 
  let dis_ok x : int = match (x: dis_single) with 
    | Result.Ok _ -> 1
    | _ -> 0
  in
  let sumdis (x: dis_single list)  = List.fold_left (+) 0 (List.map dis_ok x) in
  StringMap.map (fun ins vs -> sumdis (List.map (fun (ins, op, ds) -> ds) vs)) i 
(*  
  let elems = List.map snd (StringMap.bindings i) in
  let online = List.map (fun (ins, op, dr) -> (ins, op, dr.online)) elems in
    *)


let do_count r =  
  let v = (new branch_counter) in
  match r with 
      | _,_,Result.Ok (sl, tm) ->  visit_stmts v sl |> ignore ; Some v
      | _ -> None


let dis_ok v = match v with 
      | _,_,Result.Ok _ -> Some 1 
      | _ -> None

let dis_nok v = match v with 
      | _,_,Result.Ok _ -> None
      | _ -> Some 1

let get_tm (v:string * int * dis_single) = match v with 
    | _,_,(Result.Ok (sl,tm)) -> Some (Mtime.Span.to_uint64_ns tm)
    | _ -> None

let map_over_groups fn k  = let sls (k:grouped_res) = StringMap.map (fun (vs:(string * int * dis_single) list)  
    -> (List.map fn vs) |> List.map (Option.to_list) |> List.concat
   ) k
  in sls k

let summarise_result foldop init fn (i:grouped_res) : 'a StringMap.t = 
  StringMap.map (List.fold_left foldop init) (map_over_groups fn i) 
  

let total_tm_by_insn (i:grouped_res) = summarise_result (Int64.add) (Int64.zero) get_tm i
let max_tm_by_insn (i:grouped_res) = summarise_result max (Int64.zero) get_tm i
let num_compiled_by_insn (i:grouped_res) =  summarise_result (+) 0 dis_ok i
let num_failed_by_insn (i:grouped_res) =  summarise_result (+) 0 dis_nok i

let counts (i:grouped_res) = 
  let counted  = map_over_groups do_count i  in
  let max_complexity_of_grouped (i:grouped_res) =  StringMap.map (List.fold_left (fun a b -> max a  (b#gstmt_count + b#gexpr_count)) 0) counted in
  let max_branch_count_of_grouped (i:grouped_res) =  StringMap.map (List.fold_left ((fun a b -> max a (b#gbr_count) )) 0) counted  in
  let max_vars (i:grouped_res) =  StringMap.map (List.fold_left ((fun a b -> max a (b#g_vars) )) 0) counted  in
  max_complexity_of_grouped i, max_branch_count_of_grouped i, max_vars i

let avg_counts (i:grouped_res) = 
  let counted  = map_over_groups do_count i  in
  let num_dis  =  summarise_result (+) 0 dis_ok i in
  let tot_complexity_of_grouped =  StringMap.map (List.fold_left (fun a b -> (+) a  (b#gstmt_count + b#gexpr_count)) 0) counted in
  let tot_branch_count_of_grouped  =  StringMap.map (List.fold_left ((fun a b -> (+) a (b#gbr_count) )) 0) counted  in
  let tot_vars =  StringMap.map (List.fold_left ((fun a b -> (+) a (b#g_vars) )) 0) counted  in
  let  avg gr = StringMap.mapi (fun k v -> Float.div (Float.of_int v)  (Float.of_int @@ StringMap.find k num_dis)) gr in
  (avg tot_complexity_of_grouped), (avg tot_branch_count_of_grouped), (avg tot_vars)

(*let total_complexity_by_insn (i:grouped_res) =  summarise_result (+) 0 ok_stmt_count i *)

let avg_tm_by_insn (i:grouped_res) = 
  let total = total_tm_by_insn i in
  let counts = num_compiled_by_insn i in
  StringMap.mapi (fun k _ -> (Int64.div (StringMap.find k  total) (Int64.of_int (StringMap.find k counts)))) i


let fmemoed name (fbfn : unit -> 'a) = 
  let load_from name = 
    let ic = open_in name in
    let res = Marshal.from_channel ic in
    close_in ic ; res
  in
  let store_to name data = 
    let oc = open_out name in
    Marshal.to_channel oc data [];
    close_out oc; data 
  in

  if (Sys.file_exists name) then 
    load_from name 
  else 
    store_to name (fbfn ()) 
  


let do_analysis tblname (online_by_insgrp : grouped_res) offline_by_insgroup  = 
  let passed_on = num_compiled_by_insn online_by_insgrp in
  let passed_off = num_compiled_by_insn offline_by_insgroup in
  let failed_on = num_failed_by_insn online_by_insgrp in
  let failed_off = num_failed_by_insn offline_by_insgroup in

  (* filter out the records that didn't pass in both online and offline *)
  let passedset (x: grouped_res) = 
    let x = StringMap.bindings x in
    let x = List.concat_map (fun (k, vs) -> vs) x in
    let ps = List.map (fun (ins, op, rs) -> match rs with 
        | Result.Ok _ -> Some (op)
        | _ -> None
          ) x
    in
    IntSet.of_list @@ List.concat_map Option.to_list ps
  in
  let ponline, poffline = (passedset online_by_insgrp),(passedset offline_by_insgroup)  in
  let allowed = IntSet.inter ponline poffline in
  let missing_offline = IntSet.diff ponline poffline in
  let allowedop_filter b = 
    let x = StringMap.map (fun vs -> List.filter (fun (b,op,is) -> IntSet.mem op allowed) vs) b in
    StringMap.filter (fun k vs -> (List.length vs) > 0) x
    in
  let online_by_insgrp = allowedop_filter online_by_insgrp in
  let offline_by_insgroup  = allowedop_filter offline_by_insgroup in

  let toton = total_tm_by_insn  online_by_insgrp in
  let totoff = total_tm_by_insn offline_by_insgroup in
  let to_secs_float x = StringMap.bindings x |>  List.map (fun (k, v) -> k, ns_to_ms (Int64.to_float v)) |> List.to_seq |> StringMap.of_seq in
  let total_s_ol = to_secs_float toton in 
  let total_s_off = to_secs_float totoff in

  let avg_s_ol = to_secs_float @@ avg_tm_by_insn online_by_insgrp in
  let avg_s_off = to_secs_float @@ avg_tm_by_insn offline_by_insgroup in

  let off_complexity, off_branch, off_vars = counts offline_by_insgroup  in
  let on_complexity, on_branch, on_vars = counts online_by_insgrp in
  let avg_off_complexity, avg_off_branch, avg_off_vars = avg_counts offline_by_insgroup  in
  let avg_on_complexity, avg_on_branch, avg_on_vars = avg_counts online_by_insgrp in
  let df  = 
    let open NoCSV in 
    let headsi =  (StringMap.bindings off_complexity |> List.map fst) in
    let array_of x = List.map (fun h -> StringMap.find h x) headsi in
    let vals x = List.map (fun x -> IntC x) (array_of x) in
    let fvals x = List.map (fun x -> FloatC x) (array_of x) in
    let heads =  List.map (fun x -> StringC x) headsi in
    let f = NoCSV.make (["section";
    "online dis";"offline dis";
    "online failed"; "offline failed";
    "online time total ms" ; "offline time total ms" ;
    "online time avg ms"; "offline time avg ms";
    "online max complexity"; "offline max complexity";
    "online avg complexity"; "offline avg complexity";
    "online max branch count"; "offline max branch count";
    "online avg branch count"; "offline avg branch count";
    "online max variable count"; "offline max variable count";
    "online avg variable count"; "offline avg variable count";
    ]) [] in  
      let f3 = NoCSV.add_columns f [heads;
          vals passed_on ; vals passed_off ;
          vals failed_on ; vals failed_off ;
          fvals total_s_ol ; fvals total_s_off ;
          fvals avg_s_ol ; fvals avg_s_off;
          vals on_complexity; vals off_complexity; 
          fvals avg_on_complexity; fvals avg_off_complexity; 
          vals on_branch ; vals off_branch ;
          fvals avg_on_branch ; fvals avg_off_branch ;
          vals on_vars ; vals off_vars  ;
          fvals avg_on_vars ; fvals avg_off_vars  
      ] in
      let x = open_out (tblname ^ ".csv") in
      output_string x (NoCSV.to_csv f3); 
      close_out x ;
  in missing_offline
  

module CanonicaliseNames = struct
  type var_t = {name: ident ; index: int}


  let rec prints_arg_type (t: ty) : string =
    match t with
      | (Type_Bits e) -> "BV" ^ Asl_utils.pp_expr(e)
      | (Type_Constructor (Ident "integer")) -> "I"
      | (Type_Constructor (Ident "boolean")) -> "BOOL"
      | (Type_Tuple l) -> (String.concat "_" (List.map prints_arg_type (l)))
      | Type_Constructor (Ident "rt_label") -> "L"
      | Type_Constructor (Ident "rt_sym") -> "S" 
      | Type_Constructor (Ident "rt_expr") -> "E"
      | Type_Constructor (Ident e) -> e 
      | t -> failwith @@ "Unknown arg type: " ^ (Asl_utils.pp_type t)


  let ident_for_v (e: var_t) : ident =
    if e.index = 0 then e.name else
    match e.name with
    | Ident s -> Ident (s ^ "_" ^ (string_of_int e.index))
    | FIdent (s, i) -> FIdent ((s ^ "_" ^ (string_of_int e.index), i))

  let pident ty scoped = ((prints_arg_type ty) ^ "_var" ^ (Int.to_string @@ Stack.length scoped))

  open Transforms.ScopedBindings

  class renamer (globals) = object(this)
    inherit Asl_visitor.nopAslVisitor

    val renamed_mapping: (ident) Transforms.ScopedBindings.t =  Transforms.ScopedBindings.init ()
    val scoped_bindings : (var_t) Transforms.ScopedBindings.t =  Transforms.ScopedBindings.init ()

    method push_scope (_:unit) : unit = push_scope scoped_bindings ()
    method pop_scope (_:unit) : unit = pop_scope scoped_bindings () 
    method add_bind  (t:ty) (n: var_t): unit = add_bind scoped_bindings n.name n
    method existing_binding (i: ident) : (var_t) option = match find_binding renamed_mapping i with
      | Some n -> find_binding scoped_bindings n 
      | None -> None

    method! enter_scope (vs:(ty *ident) list) =  this#push_scope () 
    method! leave_scope u =  this#pop_scope () 

    method incr_binding (i: ident) : var_t =
      let v = this#existing_binding i in
      match v with
      | Some (b) -> {b with index = b.index + 1}
      | None -> {name=i; index=0}

    method! vstmt s =
      let nident t = Ident (pident t scoped_bindings)  in
      match s with
        | Stmt_VarDeclsNoInit(ty, vs, loc) ->
            let ns = List.map this#incr_binding (List.mapi (fun i v -> Ident (pident ty scoped_bindings ^ (Int.to_string i))) vs) in
            List.iter2 (fun n v -> (Transforms.ScopedBindings.add_bind renamed_mapping v n.name)) ns vs  ;
            List.iter (this#add_bind ty) ns; DoChildren
        | Stmt_VarDecl(ty, v, i, loc) ->
            let nv = nident ty in
            let b = this#incr_binding nv in
            this#add_bind ty b; DoChildren
        | Stmt_ConstDecl(ty, v, i, loc) ->
            let nv = nident ty in
            let b = this#incr_binding nv in
            Transforms.ScopedBindings.add_bind scoped_bindings v b ;
            this#add_bind ty b; DoChildren
        (* Statements with child scopes that shouldn't appear towards the end of transform pipeline *)
        | Stmt_Case _ -> failwith "(FixRedefinitions) case not expected"
        | Stmt_For _ -> failwith "(FixRedefinitions) for not expected"
        | Stmt_While _ -> failwith "(FixRedefinitions) while not expected"
        | Stmt_Repeat _ -> failwith "(FixRedefinitions) repeat not expected"
        | Stmt_Try _ -> failwith "(FixRedefinitions) try not expected"
        | _ -> DoChildren

    method! vlvar e =
       (match (this#existing_binding e) with
          | Some e -> ChangeTo (ident_for_v e)
          | None -> SkipChildren)

    method! vvar e =
       (match (this#existing_binding e) with
          | Some e -> ChangeTo (ident_for_v e)
          | None -> SkipChildren)
    end

  let run (g: Asl_utils.IdentSet.t) (s:stmt list) : stmt list =
    let v = new renamer g in
    visit_stmts v s
end



let main () = 

  Printf.printf "Online starting..." ;
  flush stdout;
  let online = fmemoed "online.marsh" (fun _ -> lift_online (all_insn_opcodes ())) in
  Printf.printf "done\n";
  Printf.printf "Offline starting..." ;
  flush stdout;
  let offline = fmemoed "offline.marsh" (fun _ -> lift_offline (all_insn_opcodes ())) in
  Printf.printf "done\n";
  flush stdout;

  let snddash x = (String.index_from x (1 + String.index  x '_') '_') in
  let insname x = String.sub x 0 (snddash x) in

  let by_ins x = group_list (fun (ins,op, dis) -> ins) x in

  let tot = total_tm_by_insn ((by_ins online)) in
  let ptbl name x = 
    Printf.printf "%s\n" name ;
    StringMap.iter (fun k v -> Printf.printf "%s,%f\n" k (ns_to_s @@ Int64.to_float v)) x
  in
  ptbl "total by ins" tot;

  let online_by_insgrp = (group_list (fun (ins, op,dis) -> insname ins) online) in
  let offline_by_insgroup = (group_list (fun (ins, op,dis) -> insname ins) offline ) in
  do_analysis "by_instruction_group" online_by_insgrp offline_by_insgroup |> ignore;
  let online_by_insgrp = (group_list (fun (ins, op,dis) -> ins) online) in
  let offline_by_insgroup = (group_list (fun (ins, op,dis) -> ins) offline ) in
  do_analysis "by_instruction" online_by_insgrp offline_by_insgroup |> ignore;
  let to_sm x = StringMap.of_seq (List.to_seq (List.map (fun (ins, op,dis) -> (hex_of_int op), [(ins,op,dis)]) x)) in
  let offline_by_opcode = to_sm offline in 
  let (online_by_opcode : (bitsLit * i * dis_single) list StringMap.t) = to_sm online in

  let missing_offline = do_analysis "by_opcode" online_by_opcode offline_by_opcode in
  IntSet.iter (fun x -> 
    List.iter (fun (bl,i,ds) -> Printf.printf "Missing offline: %s : %x\n" bl i) (StringMap.find (hex_of_int x) offline_by_opcode)
  ) missing_offline; 

  let dump_group pref g (rs:grouped_res) = 
    let to_dump = StringMap.find g rs in
    let progs : stmt list list = List.map (function 
      | (_,_,Result.Ok (b,_)) -> CanonicaliseNames.run Asl_utils.IdentSet.empty b
      | b -> []
    ) to_dump in
    let progs = List.map (function 
      | [] -> "error"
      | b -> String.concat "\n" (List.map (fun x -> Utils.to_string (Asl_parser_pp.pp_raw_stmt x))  b)
    ) progs in
    let progs  = List.map (String.map (function 
      | ';' -> ','
      | c -> c
    )) progs 
    in
    let progs = List.combine progs (List.map (function (_,op,_) -> op) to_dump) in
    List.iter (fun (prog, op) -> 
      let oc = open_out (pref ^ g ^ ":" ^ (hex_of_int op)) in
      output_string oc prog ; close_out oc
    ) progs 
  in
    dump_group "comparout/offline-" "aarch64_vector_arithmetic_unary_special_sqrt_est_fp16_simd" offline_by_insgroup |> ignore;
    dump_group "comparout/online-" "aarch64_vector_arithmetic_unary_special_sqrt_est_fp16_simd" online_by_insgrp |> ignore;
    dump_group "comparout/offline-" "aarch64_float_arithmetic_round_frint" offline_by_insgroup |> ignore ;
    dump_group "comparout/online-" "aarch64_float_arithmetic_round_frint" online_by_insgrp |> ignore ;
  ()


let () = main () |> ignore
