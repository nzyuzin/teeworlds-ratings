exception UnknownExternalRequest of string
exception UnknownDataRequest of string
exception UnknownRegistrationRequest of string

type data_request =
  | Players_by_rating of int64 * int64
  | Player_info of string
  | Clan_info of string
  | Game_info of int64
  | Games_by_date of int64 * int64

type registration_request =
  | Register of string
  | Name_available of string

type external_message =
  | Data_request of data_request
  | Registration_request of registration_request

type data_request_response =
  | Players_by_rating of int64 * Player.t list
  | Player_info of {
    player: Player.t;
    clan_name: string;
    stats: Player_stats.t;
    total_games: int64;
    games_with_rating_change: (Game.t * int64) list;
  }
  | Clan_info of {clan: Clan.t; players: Player.t list; average_rating: int64}
  | Game_info of Game.t * ((Game_player.t * string) list)
  | Games_by_date of int64 * Game.t list

type registration_request_response =
  | Register
  | Name_available of bool

type external_message_response =
  | Data_request_response of data_request_response
  | Registration_request_response of registration_request_response

let wrap_int (i: int64) =
  `Int(Int64.to_int i)

let players_by_rating_of_json: Json.t -> data_request = function
  | `Assoc([
      ("limit", `Int(limit));
      ("offset", `Int(offset));
    ]) -> Players_by_rating (Int64.of_int limit, Int64.of_int offset)
  | something_else -> raise (Json.error_ill_formed "players_by_rating" something_else)

let player_info_of_json: Json.t -> data_request = function
  | `Assoc([
      ("name", `String(name));
    ]) -> Player_info name
  | something_else -> raise (Json.error_ill_formed "player_info" something_else)

let clan_info_of_json: Json.t -> data_request = function
  | `Assoc([
      ("name", `String(name));
    ]) -> Clan_info name
  | something_else -> raise (Json.error_ill_formed "clan_info" something_else)

let game_info_of_json: Json.t -> data_request = function
  | `Assoc([
      ("game_id", `Int(id));
    ]) -> Game_info (Int64.of_int id)
  | something_else -> raise (Json.error_ill_formed "game_info" something_else)

let games_by_date_of_json: Json.t -> data_request = function
  | `Assoc([
      ("limit", `Int(limit));
      ("offset", `Int(offset));
    ]) -> Games_by_date (Int64.of_int limit, Int64.of_int offset)
  | something_else -> raise (Json.error_ill_formed "games_by_date" something_else)


let data_request_of_json: Json.t -> data_request = function
  | `Assoc([
      ("data_request_type", `String(data_request_type));
      ("data_request_content", body);
    ]) -> begin match data_request_type with
        | "players_by_rating" -> players_by_rating_of_json body
        | "player_info" -> player_info_of_json body
        | "clan_info" -> clan_info_of_json body
        | "game_info" -> game_info_of_json body
        | "games_by_date" -> games_by_date_of_json body
        | something_else -> raise (UnknownDataRequest something_else)
      end
  | something_else -> raise (Json.error_ill_formed "data_request" something_else)

let name_available_of_json: Json.t -> registration_request = function
  | `Assoc([
      ("name", (`String name));
    ]) -> Name_available name
  | something_else -> raise (Json.error_ill_formed "name_available" something_else)

let register_of_json: Json.t -> registration_request = function
  | `Assoc([
      ("name", `String(name));
    ]) -> Register name
  | something_else -> raise (Json.error_ill_formed "register" something_else)

let registration_request_of_json: Json.t -> registration_request = function
  | `Assoc([
      ("registration_request_type", `String(registration_request_type));
      ("registration_request_content", body);
    ]) -> begin match registration_request_type with
        | "name_available" -> name_available_of_json body
        | "register" -> register_of_json body
        | something_else -> raise (UnknownRegistrationRequest something_else)
      end
  | something_else -> raise (Json.error_ill_formed "registration_request" something_else)

let external_message_of_json: Json.t -> external_message = function
  | `Assoc([
      ("external_message_type", `String(message_type));
      ("external_message_content", rest);
    ]) -> begin match message_type with
        | "data_request" -> Data_request (data_request_of_json rest)
        | "registration_request" -> Registration_request (registration_request_of_json rest)
        | something_else -> raise (UnknownExternalRequest something_else)
      end
  | something_else -> raise (Json.error_ill_formed "teeworlds_message" something_else)

let json_of_db_player: Player.t -> Json.t = function
  | {Player.name = nm; Player.clan_id = cn; Player.rating = rtng; Player.secret_key} ->
      `Assoc([
        ("name", `String(nm));
        ("clan_id", `Int(Int64.to_int cn));
        ("rating", `Int(Int64.to_int rtng));
        ("secret_key", `String(secret_key));
      ])

let json_of_db_clan: Clan.t -> Json.t = function
  | {Clan.name = nm; Clan.description = dscr; Clan.sub_clan_id = sid} ->
      `Assoc([
        ("clan_name", `String(nm));
        ("description", `String(dscr));
        ("sub_clan_id", `Int(Int64.to_int sid));
      ])

let json_of_db_game: Game.t -> Json.t = let open Game in function
  | {game_id = id; gametype = gt; map = mp; game_time = gtime; game_result = res; game_date = date} ->
      `Assoc([
        ("game_id", `Int(Int64.to_int id));
        ("gametype", `String(gt));
        ("map", `String(mp));
        ("game_time", `Int(Int64.to_int gtime));
        ("game_result", `String(res));
        ("game_date", `String(date));
      ])

let json_of_db_player_game: (Game.t * int64) -> Json.t = let open Game in function
  | ({game_id = id; gametype = gt; map = mp; game_time = gtime; game_result = res; game_date = date}, rating_change) ->
      `Assoc([
        ("game_id", `Int(Int64.to_int id));
        ("gametype", `String(gt));
        ("map", `String(mp));
        ("game_time", `Int(Int64.to_int gtime));
        ("game_result", `String(res));
        ("game_date", `String(date));
        ("rating_change", `Int(Int64.to_int rating_change));
      ])

let json_of_player_stats: Player_stats.t -> Json.t = let open Player_stats in function
  | {
      hammer_kills = hammer_kills;
      gun_kills = gun_kills;
      shotgun_kills = shotgun_kills;
      grenade_kills = grenade_kills;
      rifle_kills = rifle_kills;
      deaths = deaths;
      suicides = suicides;
      flag_grabs = flag_grabs;
      flag_captures = flag_captures;
      flag_returns = flag_returns;
      flag_carrier_kills = flag_carrier_kills;
    } -> `Assoc([
        ("hammer_kills", wrap_int(hammer_kills));
        ("gun_kills", wrap_int(gun_kills));
        ("shotgun_kills", wrap_int(shotgun_kills));
        ("grenade_kills", wrap_int(grenade_kills));
        ("rifle_kills", wrap_int(rifle_kills));
        ("deaths", wrap_int(deaths));
        ("suicides", wrap_int(suicides));
        ("flag_grabs", wrap_int(flag_grabs));
        ("flag_captures", wrap_int(flag_captures));
        ("flag_returns", wrap_int(flag_returns));
        ("flag_carrier_kills", wrap_int(flag_carrier_kills));
      ])

let json_of_db_game_player: (Game_player.t * string) -> Json.t = let open Game_player in function
  | ({ game_id = _;
      player_id = _;
      score = scr;
      team = tm;
      rating_change = rchng;
      stats = stats;
    }, player_name) -> `Assoc([
        ("player_name", `String(player_name));
        ("score", wrap_int(scr));
        ("team", `String(tm));
        ("rating_change", wrap_int(rchng));
        ("stats", json_of_player_stats stats);
      ])

let json_of_data_request_response: data_request_response -> Json.t = function
  | Players_by_rating (total_players, players) ->
      `Assoc([
        ("data_request_response_type", `String("players_by_rating"));
        ("data_request_response_content", `Assoc([
          ("total_players", wrap_int(total_players));
          ("players", `List(List.map json_of_db_player players));
        ]));
      ])
  | Player_info {
      player = player;
      clan_name = clan_name;
      stats = stats;
      total_games = total_games;
      games_with_rating_change = games
    } ->
      `Assoc([
        ("data_request_response_type", `String("player_info"));
        ("data_request_response_content", `Assoc([
          ("player", json_of_db_player player);
          ("clan_name", `String(clan_name));
          ("stats", json_of_player_stats stats);
          ("total_games", wrap_int(total_games));
          ("games", `List(List.map json_of_db_player_game games));
        ]));
      ])
  | Clan_info {clan = clan; players = players; average_rating = average_rating} ->
      `Assoc([
        ("data_request_response_type", `String("clan_info"));
        ("data_request_response_content", `Assoc([
          ("clan", json_of_db_clan clan);
          ("players", `List(List.map json_of_db_player players));
          ("average_rating", `Int(Int64.to_int average_rating));
        ]));
      ])
  | Game_info (game, participants) ->
      `Assoc([
        ("data_request_response_type", `String("game_info"));
        ("data_request_response_content", `Assoc([
          ("game", json_of_db_game game);
          ("participants", `List(List.map json_of_db_game_player participants));
        ]));
      ])
  | Games_by_date (total_games, games) ->
      `Assoc([
        ("data_request_response_type", `String("games_by_date"));
        ("data_request_response_content", `Assoc([
          ("total_games", wrap_int total_games);
          ("games", `List(List.map json_of_db_game games));
        ]));
      ])

let json_of_registration_request_response: registration_request_response -> Json.t = function
  | Register -> `Assoc([
      ("registration_request_response_type", `String("register"));
      ("registration_request_response_content", `String("OK"));
    ])
  | Name_available answer -> `Assoc([
      ("registration_request_response_type", `String("name_available"));
      ("registration_request_response_content", `Bool(answer));
    ])

let json_of_external_message_response: external_message_response -> Json.t = function
  | Data_request_response content -> `Assoc([
      ("external_message_type", `String("data_request_response"));
      ("external_message_content", json_of_data_request_response content);
    ])
  | Registration_request_response content -> `Assoc([
      ("external_message_type", `String("registration_request_response"));
      ("external_message_content", json_of_registration_request_response content);
    ])
