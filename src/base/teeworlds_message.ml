(*
 * Message format. Each string on the separate line below is expected to be
 * seen on the new line in the message. Angle braces denote placeholders for
 * the message text.
 *
 * General message format
 * <message_type>
 * <rest of the message>
 *
 * Gameinfo format is explained in src/base/gameinfo.ml
 *
 * Player_request format
 * <player_request_type>
 * <client_id: int>
 * <rest of the player request>
 *
 * Player_rank format
 * <player_name: string>
 *
 * Top5_players format
 * <>
 *
 * Login format
 * <secret_key: string>
 *)

exception IllFormattedMessage of string
exception UnknownMessageType of string

type player_request =
  | Player_rank of string
  | Player_stats of string
  | Top5_players
  | Login of string * string

type t =
  | Gameinfo of Gameinfo.t
  | Player_request of player_request * int

type server_response =
  | Acknowledge
  | Callback of string

let string_of_player_request = function
  | Player_rank _ -> "Player_rank"
  | Top5_players -> "Top5_players"
  | Login (_, _) -> "Login"
  | Player_stats _ -> "Player_stats"

let parse_player_rank msg =
  let player_line = Stream.next msg in
  let player_name = Parser.read_quoted_word player_line 0 in
  Player_rank (Parser.unguard_quotes player_name)

let parse_login msg =
  let player_line = Stream.next msg in
  let player_name = Parser.read_quoted_word player_line 0 in
  let secret_key_line = Stream.next msg in
  Login (player_name, secret_key_line)

let parse_stats msg =
  let player_line = Stream.next msg in
  let player_name = Parser.read_quoted_word player_line 0 in
  Player_stats (Parser.unguard_quotes player_name)

let parse_player_request msg =
  let player_request_type_str = Stream.next msg in
  let client_id_str = Stream.next msg in
  let client_id = int_of_string client_id_str in
  let parsed_player_request =
    match player_request_type_str with
    | "Player_rank" -> parse_player_rank msg
    | "Top5_players" -> Top5_players
    | "Login" -> parse_login msg
    | "Player_stats" -> parse_stats msg
    | other_type -> raise (UnknownMessageType other_type)
  in
  Player_request (parsed_player_request, client_id)

let parse_message (msg_string: string) =
  let line_stream_of_string str =
    Stream.of_list (Str.split (Str.regexp "\n") str) in
  let msg = line_stream_of_string msg_string in
  try
    let msg_type = Stream.next msg in
    match msg_type with
    | "Gameinfo" -> Gameinfo (Gameinfo.parse_gameinfo msg)
    | "Player_request" -> parse_player_request msg
    | other_type -> raise (UnknownMessageType other_type)
  with
  | Stream.Failure -> raise (IllFormattedMessage msg_string)
  | Scanf.Scan_failure str ->
      raise (IllFormattedMessage ("Couldn't read '" ^ str ^ "' in " ^ msg_string))

let json_of_stats: Gameinfo.player_stats -> Json.t = let open Gameinfo in function
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
        ("hammer_kills", `Int(hammer_kills));
        ("gun_kills", `Int(gun_kills));
        ("shotgun_kills", `Int(shotgun_kills));
        ("grenade_kills", `Int(grenade_kills));
        ("rifle_kills", `Int(rifle_kills));
        ("deaths", `Int(deaths));
        ("suicides", `Int(suicides));
        ("flag_grabs", `Int(flag_grabs));
        ("flag_captures", `Int(flag_captures));
        ("flag_returns", `Int(flag_returns));
        ("flag_carrier_kills", `Int(flag_carrier_kills));
      ])

let rec json_of_players (players: Gameinfo.player list) =
  match players with
  | [] -> []
  | player :: rest -> let open Json in let open Gameinfo in
    `Assoc([
      ("name", `String(player.name));
      ("score", `Int(player.score));
      ("team", `String(string_of_team(player.team)));
      ("stats", json_of_stats player.stats);
    ]) :: (json_of_players rest)

let json_of_gameinfo (gameinfo: Gameinfo.t) = let open Gameinfo in
  `Assoc([
    ("gametype", `String(string_of_gametype gameinfo.gametype));
    ("map", `String(gameinfo.map));
    ("time", `Int(gameinfo.time));
    ("game_result", `String(string_of_game_result(gameinfo.game_result)));
    ("players", `List(json_of_players gameinfo.players))
  ])

let stats_of_json: Json.t -> Gameinfo.player_stats = let open Gameinfo in function
  | `Assoc([
        ("hammer_kills", `Int(hammer_kills));
        ("gun_kills", `Int(gun_kills));
        ("shotgun_kills", `Int(shotgun_kills));
        ("grenade_kills", `Int(grenade_kills));
        ("rifle_kills", `Int(rifle_kills));
        ("deaths", `Int(deaths));
        ("suicides", `Int(suicides));
        ("flag_grabs", `Int(flag_grabs));
        ("flag_captures", `Int(flag_captures));
        ("flag_returns", `Int(flag_returns));
        ("flag_carrier_kills", `Int(flag_carrier_kills));
      ]) -> {
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
        }
  | json -> raise (Json.error_ill_formed "player_stats" json)

let rec players_of_json (players: Json.t list): Gameinfo.player list =
  match players with
  | [] -> []
  | `Assoc([
      ("name", `String(nm));
      ("score", `Int(scr));
      ("team", `String(tm));
      ("stats", stats_json);
    ]) :: rest ->
      let open Gameinfo in
      let player = {
        name = nm;
        score = scr;
        team = team_of_string tm;
        stats = stats_of_json stats_json;
      } in
      player :: (players_of_json rest)
  | json :: rest -> raise (Json.error_ill_formed "player" json)

let gameinfo_of_json (gameinfo: Json.t): t =
  match gameinfo with
  | `Assoc([
      ("gametype", `String(gt));
      ("map", `String(map));
      ("time", `Int(time));
      ("game_result", `String(rslt));
      ("players", `List(plrs))
    ]) -> Gameinfo {
        Gameinfo.gametype = Gameinfo.gametype_of_string gt;
        Gameinfo.map = map;
        Gameinfo.time = time;
        Gameinfo.game_result = Gameinfo.game_result_of_string rslt;
        Gameinfo.players = players_of_json plrs
      }
  | json -> raise (Json.error_ill_formed "gameinfo" json)

let player_rank_of_json: Json.t -> player_request = function
  | `Assoc([
      ("player_name", `String(name));
    ]) -> Player_rank name
  | something_else -> raise (Json.error_ill_formed "player_rank" something_else)

let json_of_player_request: player_request -> Json.t = function
  | Player_rank name -> `Assoc([
      ("player_name", `String(name));
    ])
  | Top5_players -> `String("")
  | Login (name, secret_key) -> `Assoc([
      ("player_name", `String(name));
      ("secret_key", `String(secret_key));
    ])
  | Player_stats name -> `Assoc([
      ("player_name", `String(name));
    ])

let login_of_json: Json.t -> player_request = function
  | `Assoc([
      ("player_name", `String(name));
      ("secret_key", `String(secret_key));
    ]) -> Login (name, secret_key)
  | something_else -> raise (Json.error_ill_formed "login" something_else)

let stats_of_json: Json.t -> player_request = function
  | `Assoc([
      ("player_name", `String(name));
    ]) -> Player_stats name
  | something_else -> raise (Json.error_ill_formed "player_stats" something_else)

let player_request_of_json: Json.t -> t = function
  | `Assoc([
      ("player_request_type", `String(player_request_type));
      ("client_id", `Int(client_id));
      ("player_request_content", rest);
    ]) -> begin
        match player_request_type with
        | "Player_rank" -> Player_request (player_rank_of_json rest, client_id)
        | "Top5_players" -> Player_request (Top5_players, client_id)
        | "Login" -> Player_request (login_of_json rest, client_id)
        | "Player_stats" -> Player_request (stats_of_json rest, client_id)
        | something_else ->
            raise (Json.error_unknown_value "player request type" something_else)
      end
  | something_else -> raise (Json.error_ill_formed "player_request" something_else)

let json_of_teeworlds_message: t -> Json.t = function
  | Gameinfo gameinfo ->
     `Assoc([
        ("teeworlds_message_type", `String("Gameinfo"));
        ("teeworlds_message_content", json_of_gameinfo gameinfo);
     ])
  | Player_request (player_request, clid) ->
      `Assoc([
        ("teeworlds_message_type", `String("Player_request"));
        ("teeworlds_message_content",
          `Assoc([
            ("player_request_type",
              `String(string_of_player_request player_request));
            ("client_id", `Int(clid));
            ("player_request_content", json_of_player_request player_request);
          ]));
      ])

let teeworlds_message_of_json (json: Json.t): t =
  match json with
  | `Assoc([
      ("teeworlds_message_type", `String(message_type));
      ("teeworlds_message_content", rest);
    ]) -> begin
        match message_type with
        | "Gameinfo" -> gameinfo_of_json rest
        | "Player_request" -> player_request_of_json rest
        | something_else ->
            raise (Json.error_unknown_value "teeworlds_message type" something_else)
      end
  | something_else -> raise (Json.error_ill_formed "teeworlds_message" something_else)

let json_of_server_response: server_response -> Json.t = function
  | Acknowledge -> `Assoc([
      ("teeworlds_message_type", `String("Acknowledge"));
      ("teeworlds_message_content", `String(""));
    ])
  | Callback str -> `Assoc([
      ("teeworlds_message_type", `String("Callback"));
      ("teeworlds_message_content", `String(str));
    ])

let server_response_of_json: Json.t -> server_response = function
  | `Assoc([
      ("teeworlds_message_type", `String("Acknowledge"));
      ("teeworlds_message_content", `String(""));
    ]) -> Acknowledge
  | `Assoc([
      ("teeworlds_message_type", `String("Callback"));
      ("teeworlds_message_content", `String(str));
    ]) -> Callback str
  | something_else -> raise (Json.error_ill_formed "server_response" something_else)

