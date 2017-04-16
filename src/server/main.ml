let default_address = Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 12488)

let process_message (msg: string): unit =
  let open Gameinfo in
  let json = Json.json_of_string msg in
  let gameinfo = Json.gameinfo_of_json json in
  let players = gameinfo.players in
  List.iter (fun player -> Db.insert_player player) players

let handle_connection (input: in_channel) (output: out_channel): unit =
  let msg = Std.input_all input in
  let _ = process_message msg in
  let _ = Unix.shutdown_connection input in
  close_in input

let _ =
  Unix.establish_server handle_connection default_address
