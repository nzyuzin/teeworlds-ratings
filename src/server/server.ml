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
    Player.select player.Gameinfo.name in
  let update_rating existing_player =
    let new_rating = Rating.calculate_new_rating
      player game_id gameinfo.Gameinfo.game_result in
    Int64.sub new_rating existing_player.Player.ctf_rating in
  let lambda = match select_player player with
  | None -> raise (UnknownPlayer player.Gameinfo.name)
  | Some existing_player -> (fun () -> update_rating existing_player) in
  let _ = Game_player.insert player game_id in
  lambda

let process_gameinfo (gameinfo: Gameinfo.gameinfo) (db: string): unit =
  let players = gameinfo.Gameinfo.players in
  let _ = Db.open_db db in
  let _ = Db.begin_transaction () in
  let _ = try
    let game_id = Game.insert gameinfo in
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
    Db.commit_transaction ()
  with
  | error -> let _ = Db.rollback_transaction () in
    raise error in
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
  let top5_players = Player.select_top5 () in
  let top5_players_len = List.length top5_players in
  let combine_name_rating prev p =
    prev ^ " \"" ^ (String.escaped p.Player.name) ^ "\" " ^ (Int64.to_string p.Player.ctf_rating) in
  let name_ratings = List.fold_left combine_name_rating "" top5_players in
  let command = "_cb_report_top5 " ^ (string_of_int clid) ^ " " ^
    name_ratings ^ (minus_ones (5 - top5_players_len)) in
  command

let process_player_request pr clid db =
  let _ = Db.open_db_read_only db in
  let callback_command = match pr with
  | Teeworlds_message.Player_rank name -> begin
      match (Player.select_with_ctf_rank name) with
      | None -> report_player_rank clid name Int64.minus_one Int64.minus_one
      | Some (player, rank) -> report_player_rank clid name player.Player.ctf_rating rank
    end
  | Teeworlds_message.Top5_players -> report_top5 clid
  | Teeworlds_message.Login (name, secret) ->
      let player = Player.select_with_secret name in begin
        match player with
        | Some p when p.Player.secret_key = secret ->
            "_cb_auth_player " ^ (string_of_int clid) ^ " \"" ^ (String.escaped name) ^ "\""
        | _ -> "_cb_bad_auth " ^ (string_of_int clid)
      end
  | Teeworlds_message.Player_stats name ->
      match Player_stats.select name with
      | Some (stats, total_games) -> let open Player_stats in
        "_cb_player_stats " ^
        (string_of_int clid) ^ " " ^
        (Int64.to_string total_games) ^ " " ^
        (Int64.to_string stats.hammer_kills) ^ " " ^
        (Int64.to_string stats.gun_kills) ^ " " ^
        (Int64.to_string stats.shotgun_kills) ^ " " ^
        (Int64.to_string stats.grenade_kills) ^ " " ^
        (Int64.to_string stats.rifle_kills) ^ " " ^
        (Int64.to_string stats.deaths) ^ " " ^
        (Int64.to_string stats.suicides) ^ " " ^
        (Int64.to_string stats.flag_grabs) ^ " " ^
        (Int64.to_string stats.flag_returns) ^ " " ^
        (Int64.to_string stats.flag_captures) ^ " " ^
        (Int64.to_string stats.flag_carrier_kills)
      | None -> raise NotFound in
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
  let _ = Db.open_db_read_only db in
  let result = match msg with
  | External_messages.Players_by_rating (limit, offset) ->
      let hundred = Int64.of_int 100 in
      let bounded_limit = if Int64.compare limit hundred > 0 then hundred else limit in
      let players = Player.select_by_rating bounded_limit offset in
      let players_count = Player.count () in
      External_messages.Players_by_rating {
        total_players = players_count;
        players = players
      }
  | External_messages.Player_info name ->
      let p = Player.select_with_secret name in
      begin match p with
        | Some player ->
          let games = Game.select_latest_by_player name 10 in
          let (stats, total_games) = Option.get (Player_stats.select name) in
          let clan = Clan.select player.Player.clan_id in
          let clan_name = if Option.is_none clan then "" else (Option.get clan).Clan.name in
          External_messages.Player_info {
            player = player;
            clan_name = clan_name;
            stats = stats;
            total_games = total_games;
            games_with_rating_change = games;
          }
        | None -> raise NotFound
      end
  | External_messages.Clan_info name ->
      let c = Clan.select_by_name name in
      begin match c with
        | Some clan ->
          let players = Player.select_by_clan clan.Clan.id in
          let average_rating = Clan.select_average_rating clan.Clan.id in
          External_messages.Clan_info {
            clan = clan;
            average_rating = average_rating;
            players = players;
          }
        | None -> raise NotFound
      end
  | External_messages.Game_info id ->
      let attach_name plr =
        begin match Player.select_by_id plr.Game_player.player_id with
          | None -> raise (Failure "Player is not found by game player id!")
          | Some player -> (plr, player.Player.name)
        end in
      let game = begin match Game.select id with
          | None -> raise NotFound
          | Some gm -> gm
        end in
      let players = Game_player.select_by_game id in
      let players_with_names = List.map attach_name players in
      External_messages.Game_info (game, players_with_names)
  | External_messages.Games_by_date (limit, offset) ->
      let hundred = Int64.of_int 100 in
      let bounded_limit = if Int64.compare limit hundred > 0 then hundred else limit in
      let games = Game.select_latest bounded_limit offset in
      let games_count = Game.count () in
      External_messages.Games_by_date (games_count, games) in
  let _ = Db.close_db () in
  result

let process_registration_request (rr: External_messages.registration_request) db: External_messages.registration_request_response =
  let result = match rr with
  | External_messages.Name_available name ->
      let _ = Db.open_db_read_only db in
      External_messages.Name_available (None == (Player.select name))
  | External_messages.Register name ->
      let _ = Db.open_db db in
      let _ = Db.begin_transaction () in
      let player_id = try
        let _ = Random.self_init () in
        let new_secret = string_of_int (Random.bits ()) in
        let res = Player.insert
          {
            Player.id = Int64.minus_one;
            Player.name = name;
            Player.clan_id = Int64.minus_one;
            Player.ctf_rating = Int64.minus_one;
            Player.dm_rating = Int64.minus_one;
            Player.secret_key = new_secret;
          } in
        let _ = Db.commit_transaction () in
        res
      with
      | error -> let _ = Db.rollback_transaction () in
        raise error in
      External_messages.Success player_id
  | External_messages.Register_clan {name = name; clan_leader = clan_leader_id} ->
      let _ = Db.open_db db in
      let _ = Db.begin_transaction () in
      let clan_id = try
        let clan_id = Clan.insert { (Clan.empty ()) with Clan.name = name } in
        let _ = Player.update_clan clan_leader_id clan_id in
        let _ = Db.commit_transaction () in
        clan_id
      with
      | error -> let _ = Db.rollback_transaction () in
        raise error in
      External_messages.Success clan_id
  | External_messages.Join_clan {player_id = player_id; clan_id = clan_id} ->
      let _ = Db.open_db db in
      let _ = Db.begin_transaction () in
      let _ = try
        let _ = Player.update_clan player_id clan_id in
        Db.commit_transaction ()
      with
      | error -> let _ = Db.rollback_transaction () in
        raise error in
      External_messages.Success clan_id in
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
  let report_error str e =
    let error_message = Json.of_message (Json.Error str) in
    let _ = Json.to_channel out_conn error_message in
    let _ = output_char out_conn '\n' in
    let _ = flush out_conn in
    let _ = Network.close_connection conn in
    raise e in
  try
    let msg_line = input_line (Network.in_connection conn) in
    let msg = Json.from_string msg_line in
    let response = process_message msg db in
    let _ = Json.to_channel out_conn response in
    let _ = output_char out_conn '\n' in
    let _ = flush out_conn in
    Network.close_connection conn
  with
  | Failure str as e -> report_error ("Failure: " ^ str) e
  | Sqlite3.Error str as e -> report_error ("Database error: " ^ str) e
  | UnknownPlayer name as e -> report_error ("Player with name " ^ name ^ " is not registered") e
  | NotFound as e -> report_error "Requested entity is not found" e
  | UnknownMessageType str as e -> report_error ("Received message with unknown type: " ^ str) e
  | e -> report_error "Internal server error" e

let run (addr, port) db =
  let addr = Unix.ADDR_INET (Unix.inet_addr_of_string addr, port) in
  Network.establish_server (fun conn -> handle_connection conn db) addr
