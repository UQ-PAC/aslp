open Yojson
open String
open List
open Hashtbl



let channelprefix = "aslp" 

let query (host: string) (port:int ) (insns: string list) :  string list =
  let open Unix in
  let open Redis_sync.Client in
  let result_conn = connect {host=host; port=port} in
  let query_conn= connect {host=host; port=port}  in
  let query_channel: string = channelprefix ^ "." ^ (string_of_int @@ Unix.getpid ()) in
  let resp_channel: string = "reply." ^ query_channel in
  let print_stream_value v =
    let print_value f = Printf.printf "%s" (string_of_reply f) in
    List.iter print_value v;
    print_endline "";
  in
  psubscribe result_conn [resp_channel];
  List.iter (fun x -> (publish query_conn query_channel x |> ignore)) insns; 
  let stream = (stream result_conn) in
  let completed = Hashtbl.create (List.length insns) in
  while (Hashtbl.length completed < List.length insns) do
    let response = Stream.next stream in
    match response with 
      | [(`Bulk (Some "pmessage")); _; _; (`Bulk (Some msg))] -> (
        match (Yojson.Safe.from_string(msg)) with
        | `Assoc [("instruction", `String opcode); ("semantics", `String sem)] -> Hashtbl.add completed opcode sem; 
        | `Assoc [("instruction", `String opcode); ("error", `String msg)] -> Hashtbl.add completed opcode msg; 
        |  _ -> ())
      | _ -> () 
  done;
  List.map (fun k -> Hashtbl.find completed k) insns

let () = let insns = (tl (Array.to_list Sys.argv)) 
  in List.iter2 (fun i f -> print_endline (i ^ " :: " ^ f ^ "\n")) insns (query "0.0.0.0" 6379  insns) 
