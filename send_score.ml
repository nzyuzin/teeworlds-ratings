let usage = "Usage: " ^ Sys.argv.(0) ^ " scores"

let _ =
  Arg.parse [] (fun scores -> print_endline scores) usage
