let default_address = Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 12488)

let _ =
  Unix.establish_server Server.handle_connection default_address
