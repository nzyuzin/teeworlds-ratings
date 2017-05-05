exception UnknownMessageType of string
exception UnknownPlayer of string
exception NotFound

let process_player_info player gameinfo game_id =
  (*
   * Here we organize computation with lambdas to delay execution the of the
   * actual updates. This delay is needed since if we executed it right away,
   * the information about the players who have never been seen before might
   * not be in the db. To avoid that situation we ensure that the updates will
   * always come after the inserts.
   *)
  let select_player player =
    Player_requests.select_player player.Gameinfo.name in
  let update_rating existing_player =
    let new_rating = Rating.calculate_new_rating
      player game_id gameinfo.Gameinfo.game_result in
    Int64.sub new_rating existing_player.Db.rating in
  let lambda = match select_player player with
  | None -> raise (UnknownPlayer player.Gameinfo.name)
  | Some existing_player -> (fun () -> update_rating existing_player) in
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
    prev ^ " \"" ^ (String.escaped p.Db.name) ^ "\" " ^ (Int64.to_string p.Db.rating) in
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
  | Teeworlds_message.Top5_players -> report_top5 clid
  | Teeworlds_message.Login (name, secret) ->
      let player = Player_requests.select_player_with_secret name in
      match player with
      | Some p when p.Db.secret_key = secret ->
          "_cb_auth_player " ^ (string_of_int clid) ^ " \"" ^ (String.escaped name) ^ "\""
      | _ -> "_cb_bad_auth " ^ (string_of_int clid) in
  let _ = Db.close_db () in
  callback_command

let process_teeworlds_message (msg: Json.t) (db: string): Teeworlds_message.server_response =
  match Json.teeworlds_message_of_json msg with
  | Teeworlds_message.Gameinfo gameinfo ->
      let _ = process_gameinfo gameinfo db in
      Teeworlds_message.Acknowledge
  | Teeworlds_message.Player_request (req, clid)  ->
      Teeworlds_message.Callback (process_player_request req clid db)

let process_data_request (msg: External_messages.data_request) db: External_messages.data_request_response =
  let _ = Db.open_db db in
  let result = match msg with
  | External_messages.Players_by_rating (limit, offset) -> External_messages.Players_by_rating
      (Player_requests.select_players_by_rating limit offset)
  | External_messages.Player_info name ->
      let p = Player_requests.select_player_with_secret name in
      begin match p with
        | Some player ->
          let games = Game_requests.select_latest_games_by_player name 10 in
          External_messages.Player_info (player, games)
        | None -> raise NotFound
      end
  | External_messages.Clan_info name ->
      let c = Clan_requests.select_clan name in
      begin match c with
        | Some clan ->
          let players = Player_requests.select_players_by_clan name in
          External_messages.Clan_info (clan, players)
        | None -> raise NotFound
      end
  | External_messages.Game_info id ->
      let attach_name plr =
        begin match Player_requests.select_player_by_id plr.Db.player_id with
          | None -> raise (Failure "Player is not found by game player id!")
          | Some player -> (plr, player.Db.name)
        end in
      let game = begin match Game_requests.select_game id with
          | None -> raise NotFound
          | Some gm -> gm
        end in
      let players = Game_requests.select_game_participants id in
      let players_with_names = List.map attach_name players in
      External_messages.Game_info (game, players_with_names)
  | External_messages.Games_by_date (limit, offset) -> External_messages.Games_by_date
    (Game_requests.select_latest_games limit offset) in
  let _ = Db.close_db () in
  result

let process_registration_request (rr: External_messages.registration_request) db: External_messages.registration_request_response =
  let _ = Db.open_db db in
  let result = match rr with
  | External_messages.Name_available name ->
      External_messages.Name_available (None == (Player_requests.select_player name))
  | External_messages.Register (name, clan) ->
      let _ = Random.self_init () in
      let new_secret = string_of_int (Random.bits ()) in
      let _ = Player_requests.insert_player
        {
          Db.id = Int64.minus_one;
          Db.name = name;
          Db.clan = clan;
          Db.rating = Int64.of_int 1500;
          Db.secret_key = new_secret;
        } in
      External_messages.Register in
  let _ = Db.close_db () in
  result

let process_external_message msg db: External_messages.external_message_response =
  match External_messages.external_message_of_json msg with
  | External_messages.Data_request dr ->
      External_messages.Data_request_response (process_data_request dr db)
  | External_messages.Registration_request rr ->
      External_messages.Registration_request_response (process_registration_request rr db)

let process_message (msg: Json.t) (db: string): Json.t =
  let pack_teeworlds_message json = Json.of_message (Json.Message ("teeworlds_message", json)) in
  let pack_external_message json = Json.of_message (Json.Message ("external_message", json)) in
  match Json.to_message msg with
  | Json.Message ("teeworlds_message", body) ->
      pack_teeworlds_message (Json.json_of_server_response (process_teeworlds_message body db))
  | Json.Message ("external_message", body) ->
      pack_external_message (External_messages.json_of_external_message_response (process_external_message body db))
  | Json.Error str -> Json.of_message (Json.Error "Unexpected error request")
  | Json.Message (msg_type, _) -> raise (UnknownMessageType msg_type)

let handle_connection (conn: Network.connection) (db: string): unit =
  let out_conn = Network.out_connection conn in
  let report_error str =
    let error_message = Json.of_message (Json.Error str) in
    let _ = Json.to_channel out_conn error_message in
    let _ = output_char out_conn '\n' in
    flush out_conn in
  let _ = try
    let msg_line = input_line (Network.in_connection conn) in
    let msg = Json.from_string msg_line in
    let response = process_message msg db in
    let _ = Json.to_channel out_conn response in
    output_char out_conn '\n'
  with
  | Failure str -> report_error ("Failure: " ^ str)
  | Sqlite3.Error str -> report_error ("Database error: " ^ str)
  | UnknownPlayer name -> report_error ("Player with name " ^ name ^ " is not registered")
  | NotFound -> report_error "Requested entity is not found"
  | UnknownMessageType str -> report_error ("Received message with unknown type: " ^ str)
  | _ -> report_error "Internal server error" in
  let _ = flush out_conn in
  Network.close_connection conn

let run port db =
  let addr = Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", port) in
  Network.establish_server (fun conn -> handle_connection conn db) addr
