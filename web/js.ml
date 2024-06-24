
let init input out err =
  Js_of_ocaml.Sys_js.set_channel_flusher stdout out;
  Js_of_ocaml.Sys_js.set_channel_flusher stderr err;
  Js_of_ocaml.Sys_js.set_channel_filler stdin input;
  ()

let cachedenv : LibASL.Eval.Env.t option ref = ref None
let uncachedenv = lazy (Option.get @@ LibASL.Arm_env.aarch64_evaluation_environment ())

let env () =
  match !cachedenv with
  | Some x -> x
  | None -> Lazy.force uncachedenv

let denv = lazy (LibASL.Dis.build_env @@ env ())

let print_pp = ref true

let pp_stmt () =
  if !print_pp then Asl_utils.pp_stmt else fun x -> Utils.to_string (Asl_parser_pp.pp_raw_stmt x)

let dis (x: string) =
  let stmts = LibASL.Dis.retrieveDisassembly (env ()) Lazy.(force denv) x in
  List.iter
    (fun s -> print_endline @@ pp_stmt () s)
    stmts;
  flush stdout


let () =
  print_endline "libASL_web ocaml-side...";

  let open Js_of_ocaml in
  let () = Js.export "libASL_web"
    (object%js
      (* TODO: support stdin from javascript for repl, possibly converting asli repl to lwt *)
      method init (out : string -> unit) (err : string -> unit) = init (fun () -> "") out err

      method force = let env' = env () and _ = Lazy.force denv in fun () -> Js.bytestring @@ Marshal.to_string env' []
      method cache (a: Js.js_string Js.t) = (cachedenv := Some (Marshal.from_string (Js.to_bytestring a) 0))
      method uncache = (cachedenv := None)
      method dis (x: string) = dis x
      method setDebugLevel i = (LibASL.Dis.debug_level := i)
      method setPrettyPrint (x : bool) = (print_pp := x)
     end) in
  ()
