exception PlayerNotFound of string

let db_player_of_gameinfo_player: Gameinfo.player -> Db.player = function
  {Gameinfo.name = nm; Gameinfo.clan = cn; Gameinfo.score = scr; Gameinfo.team = tm} ->
    {Db.name = nm; Db.clan = cn; Db.rating = Int64.of_int (-1)}

let process_player_info player gameinfo game_id =
  (*
   * Here we organize computation with lambdas to delay execution the of the
   * actual updates. This delay is needed since if we executed it right away,
   * the information about the players who have never been seen before might
   * not be in the db. To avoid that situation we ensure that the updates will
   * always come after the inserts.
   *)
  let lambda = match Db.select_player player.Gameinfo.name with
  | None -> let _ = Db.insert_player (db_player_of_gameinfo_player player) in
      fun () -> Int64.of_int 1500
  | Some existing_player ->
      fun () -> Rating.calculate_new_rating player game_id gameinfo.Gameinfo.game_result in
  let _ = Db.insert_game_player player game_id in
  lambda

let process_gameinfo (gameinfo: Gameinfo.gameinfo) (db: string): unit =
  let players = gameinfo.Gameinfo.players in
  let _ = Db.open_db db in
  let game_id = Db.insert_game gameinfo in
  let delayed_ratings: (unit -> int64) list =
    List.map (fun player -> process_player_info player gameinfo game_id) players in
  (*
   * Forces the computation of the delayed updates. Note that at this point all
   * inserts have already been computed since they're firstly computed and only
   * then produce empty lambda, while updates are entirely in lambdas.
   *)
  let ratings = List.map (fun get_rating -> get_rating ()) delayed_ratings in
  let _ = List.iter2 (fun player rating -> Rating.update_rating player rating) players ratings in
  Db.close_db ()

let report_player_rank rating rank clid addr =
  let conn = Teeworlds_econ.connect addr "test" in
  let command = "_cb_report_rank " ^ (string_of_int clid) ^
    " " ^ (Int64.to_string rating) ^ " " ^ (Int64.to_string rank) in
  let _ = Teeworlds_econ.execute_command conn command in
  Teeworlds_econ.disconnect conn

let process_player_request ((pr, clid, addr): Teeworlds_message.player_request * int * Network.address) db =
  let _ = Db.open_db db in
  let _ = match pr with
  | Teeworlds_message.Player_rank name -> begin
      match (Db.select_player_with_rank name) with
      | None -> report_player_rank Int64.minus_one Int64.minus_one clid addr
      | Some (player, rank) -> report_player_rank player.Db.rating rank clid addr
    end in
  Db.close_db ()

let process_message (msg: string) (db: string): unit =
  let json = Json.json_of_string msg in
  match Json.teeworlds_message_of_json json with
  | Teeworlds_message.Gameinfo gameinfo -> process_gameinfo gameinfo db
  | Teeworlds_message.Player_request (req, clid, addr)  ->
      process_player_request (req, clid, addr) db

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
