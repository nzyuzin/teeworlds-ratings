exception UnknownValue of string
exception IllFormedJson of string

type t = Yojson.Basic.json

type message = Message of string * t | Error of string

let json_pretty_to_string json = Yojson.Basic.pretty_to_string json
let from_string str = Yojson.Basic.from_string str

let error_ill_formed expected got =
  IllFormedJson ("Unexpected json for " ^ expected ^ ":\n" ^ (json_pretty_to_string got))
let error_unknown_value field_name field_value =
  UnknownValue ("Unknown " ^ field_name ^ ": " ^ field_value)

let from_channel (chan: in_channel): t = Yojson.Basic.from_channel chan
let to_channel (chan: out_channel) (json: t): unit = Yojson.Basic.to_channel chan json

let to_message: t -> message = function
  | `Assoc([
      ("message_type", `String("error"));
      ("message_content", `String(errmsg));
    ]) -> Error errmsg
  | `Assoc([
      ("message_type", `String(message_type));
      ("message_content", rest);
    ]) -> Message (message_type, rest)
  | something_else -> raise (error_ill_formed "message" something_else)

let of_message : message -> t = function
  | Message (message_type, message_content) -> `Assoc([
      ("message_type", `String(message_type));
      ("message_content", message_content);
    ])
  | Error str -> `Assoc([
      ("message_type", `String("error"));
      ("message_content", `String(str));
    ])

let json_of_stats: Gameinfo.player_stats -> t = let open Gameinfo in function
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
  | player :: rest -> let open Yojson.Basic in let open Gameinfo in
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

let stats_of_json: t -> Gameinfo.player_stats = let open Gameinfo in function
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
  | json -> raise (error_ill_formed "player_stats" json)

let rec players_of_json (players: Yojson.Basic.json list): Gameinfo.player list =
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
  | json :: rest -> raise (error_ill_formed "player" json)

let gameinfo_of_json (gameinfo: Yojson.Basic.json): Teeworlds_message.t =
  match gameinfo with
  | `Assoc([
      ("gametype", `String(gt));
      ("map", `String(map));
      ("time", `Int(time));
      ("game_result", `String(rslt));
      ("players", `List(plrs))
    ]) -> Teeworlds_message.Gameinfo {
        Gameinfo.gametype = Gameinfo.gametype_of_string gt;
        Gameinfo.map = map;
        Gameinfo.time = time;
        Gameinfo.game_result = Gameinfo.game_result_of_string rslt;
        Gameinfo.players = players_of_json plrs
      }
  | json -> raise (error_ill_formed "gameinfo" json)

let player_rank_of_json: Yojson.Basic.json -> Teeworlds_message.player_request = function
  | `Assoc([
      ("player_name", `String(name));
    ]) -> Teeworlds_message.Player_rank name
  | something_else -> raise (error_ill_formed "player_rank" something_else)

let json_of_player_request: Teeworlds_message.player_request -> Yojson.Basic.json = function
  | Teeworlds_message.Player_rank name -> `Assoc([
      ("player_name", `String(name));
    ])
  | Teeworlds_message.Top5_players -> `String("")
  | Teeworlds_message.Login (name, secret_key) -> `Assoc([
      ("player_name", `String(name));
      ("secret_key", `String(secret_key));
    ])
  | Teeworlds_message.Player_stats name -> `Assoc([
      ("player_name", `String(name));
    ])

let login_of_json: t -> Teeworlds_message.player_request = function
  | `Assoc([
      ("player_name", `String(name));
      ("secret_key", `String(secret_key));
    ]) -> Teeworlds_message.Login (name, secret_key)
  | something_else -> raise (error_ill_formed "login" something_else)

let stats_of_json: t -> Teeworlds_message.player_request = function
  | `Assoc([
      ("player_name", `String(name));
    ]) -> Teeworlds_message.Player_stats name
  | something_else -> raise (error_ill_formed "player_stats" something_else)

let player_request_of_json: Yojson.Basic.json -> Teeworlds_message.t = function
  | `Assoc([
      ("player_request_type", `String(player_request_type));
      ("client_id", `Int(client_id));
      ("player_request_content", rest);
    ]) -> begin
        match player_request_type with
        | "Player_rank" ->
            Teeworlds_message.Player_request (player_rank_of_json rest, client_id)
        | "Top5_players" ->
            Teeworlds_message.Player_request (Teeworlds_message.Top5_players, client_id)
        | "Login" ->
            Teeworlds_message.Player_request (login_of_json rest, client_id)
        | "Player_stats" ->
            Teeworlds_message.Player_request (stats_of_json rest, client_id)
        | something_else -> raise (error_unknown_value "player request type" something_else)
      end
  | something_else -> raise (error_ill_formed "player_request" something_else)

let json_of_teeworlds_message: Teeworlds_message.t -> Yojson.Basic.json = function
  | Teeworlds_message.Gameinfo gameinfo ->
     `Assoc([
        ("teeworlds_message_type", `String("Gameinfo"));
        ("teeworlds_message_content", json_of_gameinfo gameinfo);
     ])
  | Teeworlds_message.Player_request (player_request, clid) ->
      `Assoc([
        ("teeworlds_message_type", `String("Player_request"));
        ("teeworlds_message_content",
          `Assoc([
            ("player_request_type",
              `String(Teeworlds_message.string_of_player_request player_request));
            ("client_id", `Int(clid));
            ("player_request_content", json_of_player_request player_request);
          ]));
      ])

let teeworlds_message_of_json (json: Yojson.Basic.json): Teeworlds_message.t =
  match json with
  | `Assoc([
      ("teeworlds_message_type", `String(message_type));
      ("teeworlds_message_content", rest);
    ]) -> begin
        match message_type with
        | "Gameinfo" -> gameinfo_of_json rest
        | "Player_request" -> player_request_of_json rest
        | something_else -> raise (error_unknown_value "teeworlds_message type" something_else)
      end
  | something_else -> raise (error_ill_formed "teeworlds_message" something_else)

let json_of_server_response: Teeworlds_message.server_response -> t = function
  | Teeworlds_message.Acknowledge -> `Assoc([
      ("teeworlds_message_type", `String("Acknowledge"));
      ("teeworlds_message_content", `String(""));
    ])
  | Teeworlds_message.Callback str -> `Assoc([
      ("teeworlds_message_type", `String("Callback"));
      ("teeworlds_message_content", `String(str));
    ])

let server_response_of_json: t -> Teeworlds_message.server_response = function
  | `Assoc([
      ("teeworlds_message_type", `String("Acknowledge"));
      ("teeworlds_message_content", `String(""));
    ]) -> Teeworlds_message.Acknowledge
  | `Assoc([
      ("teeworlds_message_type", `String("Callback"));
      ("teeworlds_message_content", `String(str));
    ]) -> Teeworlds_message.Callback str
  | something_else -> raise (error_ill_formed "server_response" something_else)

