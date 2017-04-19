let db_player_of_gameinfo_player: Gameinfo.player -> Db.player = function
  {Gameinfo.name = nm; Gameinfo.clan = cn; Gameinfo.score = scr; Gameinfo.team = tm} ->
    {Db.name = nm; Db.clan = cn; Db.rating = Int64.of_int (-1)}

let process_player_info player gameinfo game_id =
  let lambda = match Db.select_player player.Gameinfo.name with
  | None -> let _ = Db.insert_player (db_player_of_gameinfo_player player) in
      fun () -> ()
  | Some existing_player ->
      fun () -> Rating.update_rating player game_id gameinfo.Gameinfo.winner in
  let _ = Db.insert_game_player player game_id in
  lambda

let process_message (msg: string) (db: string): unit =
  let json = Json.json_of_string msg in
  let gameinfo = Json.gameinfo_of_json json in
  let players = gameinfo.Gameinfo.players in
  let _ = Db.open_db db in
  let game_id = Db.insert_game gameinfo in
  let delayed_updates =
    List.map (fun player -> process_player_info player gameinfo game_id) players in
  let _ = List.iter (fun update -> update ()) delayed_updates in (* TODO: Explain what this code does *)
  Db.close_db ()

let handle_connection (conn: Network.connection) (db: string): unit =
  let msg = Std.input_all (Network.in_connection conn) in
  let out_conn = Network.out_connection conn in
  let _ = try
    let _ = process_message msg db in
    output_string out_conn "OK"
  with Failure str | Sqlite3.Error str as exc ->
    let _ = output_string out_conn str in
    raise exc in
  Network.close_connection conn

let run port db =
  let addr = Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", port) in
  Network.establish_server (fun conn -> handle_connection conn db) addr
