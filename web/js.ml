let () =
  print_endline "hello from js";
  print_endline @@ Int.to_string @@ List.length LibASLResources.Res.Sites.aslfiles;
  print_endline @@ LibASL.Value.(pp_value @@ from_bitsLit "100");
  print_endline @@ Virtual.V.read "boop"
