exception UnknownMessageType of string

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
  let lambda = match Player_requests.select_player player.Gameinfo.name with
  | None -> let _ = Player_requests.insert_player (db_player_of_gameinfo_player player) in
      fun () -> Int64.of_int 1500
  | Some existing_player -> fun () -> let new_rating =
      Rating.calculate_new_rating player game_id gameinfo.Gameinfo.game_result in
      Int64.sub new_rating existing_player.Db.rating in
  let _ = Game_requests.insert_game_player player game_id in
  lambda

let process_gameinfo (gameinfo: Gameinfo.gameinfo) (db: string): unit =
  let players = gameinfo.Gameinfo.players in
  let _ = Db.open_db db in
  let game_id = Game_requests.insert_game gameinfo in
  let delayed_ratings: (unit -> int64) list =
    List.map (fun player -> process_player_info player gameinfo game_id) players in
  (*
   * Forces the computation of the delayed updates. Note that at this point all
   * inserts have already been computed since they're firstly computed and only
   * then produce empty lambda, while updates are entirely in lambdas.
   *)
  let ratings = List.map (fun get_rating -> get_rating ()) delayed_ratings in
  let update_rating player rating = Rating.update_rating game_id player.Gameinfo.name rating in
  let _ = List.iter2 update_rating players ratings in
  Db.close_db ()

let report_player_rank clid player_name rating rank =
  let command = "_cb_report_rank " ^ (string_of_int clid) ^ " \""
  ^ (String.escaped player_name) ^ "\" " ^ (Int64.to_string rating)
  ^ " " ^ (Int64.to_string rank) in
  command

let report_top5 clid =
  let rec minus_ones times =
    if times = 0 then ""
    else " -1 -1" ^ (minus_ones (times - 1)) in
  let top5_players = Player_requests.select_top5_players () in
  let top5_players_len = List.length top5_players in
  let combine_name_rating prev p =
    " \"" ^ (String.escaped p.Db.name) ^ "\" " ^ (Int64.to_string p.Db.rating) ^ " " ^ prev in
  let name_ratings = List.fold_left combine_name_rating "" top5_players in
  let command = "_cb_report_top5 " ^ (string_of_int clid) ^ " " ^
    name_ratings ^ (minus_ones (5 - top5_players_len)) in
  command

let process_player_request pr clid db =
  let _ = Db.open_db db in
  let callback_command = match pr with
  | Teeworlds_message.Player_rank name -> begin
      match (Player_requests.select_player_with_rank name) with
      | None -> report_player_rank clid name Int64.minus_one Int64.minus_one
      | Some (player, rank) -> report_player_rank clid name player.Db.rating rank
    end
  | Teeworlds_message.Top5_players -> report_top5 clid in
  let _ = Db.close_db () in
  callback_command

let process_teeworlds_message (msg: Json.t) (db: string): Teeworlds_message.server_response =
  match Json.teeworlds_message_of_json msg with
  | Teeworlds_message.Gameinfo gameinfo ->
      let _ = process_gameinfo gameinfo db in
      Teeworlds_message.Acknowledge
  | Teeworlds_message.Player_request (req, clid)  ->
      Teeworlds_message.Callback (process_player_request req clid db)

let process_message (msg: Json.t) (db: string): Json.t =
  let pack_teeworlds_message json = Json.of_message (Json.Message ("teeworlds_message", json)) in
  match Json.to_message msg with
  | Json.Message ("teeworlds_message", body) ->
      pack_teeworlds_message (Json.json_of_server_response (process_teeworlds_message body db))
  | Json.Message ("external_message", body) -> raise (Failure "Not Implemented!")
  | Json.Message (msg_type, _) -> raise (UnknownMessageType msg_type)

let handle_connection (conn: Network.connection) (db: string): unit =
  let out_conn = Network.out_connection conn in
  let _ = try
    let msg_line = input_line (Network.in_connection conn) in
    let msg = Json.from_string msg_line in
    let response = process_message msg db in
    let _ = Json.to_channel out_conn response in
    output_char out_conn '\n'
  with Failure str | Sqlite3.Error str as exc ->
    let error_message = Json.json_of_server_response (Teeworlds_message.Error str) in
    let _ = Json.to_channel out_conn (Json.of_message (Json.Message ("error", error_message))) in
    let _ = output_char out_conn '\n' in
    let _ = flush out_conn in
    raise exc in
  let _ = flush out_conn in
  Network.close_connection conn

let run port db =
  let addr = Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", port) in
  Network.establish_server (fun conn -> handle_connection conn db) addr
