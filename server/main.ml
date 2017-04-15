let default_address = Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 12488)

let handle_connection (input: in_channel) (output: out_channel): unit =
  let lines_enum = Std.input_lines input in
  let _ = Enum.iter (fun s -> print_endline s) lines_enum in
  let _ = Unix.shutdown_connection input in
  close_in input

let _ =
  Unix.establish_server handle_connection default_address
