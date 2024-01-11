
open LibASL

open String
open List
open Array 
open Asl_ast
open Value
open Eval
open Asl_utils
open Resources
open Yojson


let asl_defs_text: string list = [regs_dot_asl; types_dot_asl; arch_dot_asl; 
  arch_instrs_dot_asl; arch_decode_dot_asl; aes_dot_asl; barriers_dot_asl; 
  debug_dot_asl; feature_dot_asl; hints_dot_asl; interrupts_dot_asl; 
  memory_dot_asl; stubs_dot_asl; fetchdecode_dot_asl; override_dot_asl; 
  test_dot_asl]
let prelude_text: string = prelude_dot_asl


let envinfo : Asl_ast.declaration list =
  let prelude : Asl_ast.declaration list = LoadASL.read_text prelude_text "prelude.asl" true false     in
  let mra : Asl_ast.declaration list  list   = List.map (fun t -> LoadASL.read_text t "" false false) asl_defs_text in
  List.concat (prelude :: mra)


let persistent_env     = Eval.build_evaluation_environment envinfo

let eval_instr (opcode: string) : string = 
    let praw a : string = Utils.to_string (Asl_parser_pp.pp_raw_stmt a) |> String.trim  in
    let address = None                                     in
    let res :Asl_ast.stmt list  = Dis.retrieveDisassembly ?address persistent_env (Dis.build_env persistent_env) opcode in 
    let ascii   = List.map praw res                                                 in
    let indiv (s: string) = List.init (String.length s) (String.get s) |> List.map (String.make 1)  in
    List.map indiv ascii |>  List.map (String.concat "") |> String.concat "\n"


let get_reply (jsonin: string) : string = 
  (*let json  = Yojson.Safe.from_string jsonin in *)
   match (eval_instr jsonin) with 
  | exception e ->  Yojson.Safe.to_string  (`Assoc [("instruction", `String jsonin); ("error", `String (Printexc.to_string e))])
  | x -> Yojson.Safe.to_string (`Assoc [("instruction", `String jsonin); ("semantics", `String x)] )
  | _ ->  Yojson.Safe.to_string  (`Assoc [("instruction", `String jsonin); ("error", `String "unknown")])

(*let () = ignore (List.map (fun (f: string) -> print_endline (eval_instr f)) (tl (to_list Sys.argv))) *)



module SemCache = Redis_sync.Cache (struct 
  type key = string 
  type data = string
  let cache_key (p: key) : string = p
  let cache_expiration : int option = Some(1)
  let data_of_string (d: string) = d 
  let string_of_data (d: data) = d
end)

let channel_prefix = "aslp" 

let get_semantics_withcache (host: string) (port: int) (opcode: string) : string option = 
  let open Redis_sync.Client in
  let cache_conn = connect {host=host; port=port} in
  match (SemCache.get cache_conn opcode) with
    | Some d -> Some d
    | None -> 
      match eval_instr opcode with
        | exception _ ->  None
        | x -> SemCache.set cache_conn opcode x; Some x


let subscribe_sync host port =
  let open Redis_sync.Client in

  let print_stream_value v =
    let print_value f = Printf.printf "%s" (string_of_reply f) in
    List.iter print_value v;
    print_string "\n";
    flush stdout in

  let get_response (v: string) : string = get_reply v
  in

  let cache_conn = connect {host=host; port=port} in
  let conn = connect {host=host; port=port} in
  let pub = connect {host=host; port=port} in
  let stream = (stream conn) in
  psubscribe conn [channel_prefix ^ "*"];
  while true do
    let response = Stream.next stream in
    print_stream_value response;
    match response with 
      | [(`Bulk (Some "pmessage")); (`Bulk (Some _));(`Bulk (Some channel)); (`Bulk (Some msg))] -> 
        let response_channel = "reply." ^ channel in
        print_endline response_channel;
        let resp : string = match (SemCache.get cache_conn msg) with
          | Some d -> print_endline "used cached"; d
          | None -> (let new_val = get_response msg in
            SemCache.set cache_conn msg new_val;
            new_val)
        in
        print_endline resp;
        publish pub response_channel resp;
      ()
      | _ -> ()
  done

let () = 
  match Array.to_list Sys.argv with 
    | [_; "disas"; opcode] -> (match (get_semantics_withcache "0.0.0.0" 6379 opcode) with 
      | Some x -> print_endline x
      | None -> print_endline "Error")
    | [_; "serve"] -> subscribe_sync "0.0.0.0" 6379
    | _ -> print_endline "usage:\n\tdaemon disas opcode\n\tdaemon serve"
  (*ignore (List.map (fun (f: string) -> print_endline (eval_instr f)) (tl (to_list Sys.argv))) *)
