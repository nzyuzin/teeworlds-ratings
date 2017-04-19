let db_player_of_gameinfo_player: Gameinfo.player -> Db.player = function
  {Gameinfo.name = nm; Gameinfo.clan = cn; Gameinfo.score = scr; Gameinfo.team = tm} ->
    {Db.name = nm; Db.clan = cn; Db.rating = Int64.of_int (-1)}

type game_result = Victory | Defeat

let score_of_game_result = function
  | Victory -> 1.
  | Defeat -> 0.

let elo (rating_a: int64) (rating_b: int64) (game_result: game_result): int64 =
  let a = Int64.to_float rating_a in
  let b = Int64.to_float rating_b in
  let expected_score = 1. /. (1. +. (10. ** ((b -. a) /. 400.))) in
  let k = if a >= 2000. then 10. else 20. in
  Int64.of_float (a +. (k *. ((score_of_game_result game_result) -. expected_score)))

let calculate_average_rating (players: Db.player list): int64 =
  let ratings = List.map (fun player -> player.Db.rating) players in
  List.fold_left ( Int64.add ) (Int64.of_int 0) ratings

let process_player_info player gameinfo game_id =
  let _ = match Db.select_player player.Gameinfo.name with
  | None -> let _ = Db.insert_player (db_player_of_gameinfo_player player) in
    ()
  | Some existing_player ->
      let new_rating = elo existing_player.Db.rating existing_player.Db.rating Victory in
      Db.update_rating existing_player.Db.name new_rating in
  let _ = Db.insert_game_player player game_id in
  ()

let process_message (msg: string) (db: string): unit =
  let json = Json.json_of_string msg in
  let gameinfo = Json.gameinfo_of_json json in
  let players = gameinfo.Gameinfo.players in
  let _ = Db.open_db db in
  let game_id = Db.insert_game gameinfo in
  let _ = List.iter (fun player -> process_player_info player gameinfo game_id) players in
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
