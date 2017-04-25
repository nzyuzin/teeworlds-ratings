exception UnknownValue of string
exception IllFormedJson of string

type t = Yojson.Basic.json

type message = Message of string * t

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
      ("message_type", `String(message_type));
      ("message_content", rest);
    ]) -> Message (message_type, rest)
  | something_else -> raise (error_ill_formed "message" something_else)

let of_message (Message (message_type, message_content)): t =
  `Assoc([
    ("message_type", `String(message_type));
    ("message_content", message_content);
  ])

let rec json_of_players (players: Gameinfo.player list) =
  match players with
  | [] -> []
  | player :: rest -> let open Yojson.Basic in
    `Assoc([
      ("name", `String(player.Gameinfo.name));
      ("clan", `String(player.Gameinfo.clan));
      ("score", `Int(player.Gameinfo.score));
      ("team", `String(Gameinfo.string_of_team(player.Gameinfo.team)))
    ]) :: (json_of_players rest)

let json_of_gameinfo (gameinfo: Gameinfo.gameinfo) =
  `Assoc([
    ("gametype", `String(gameinfo.Gameinfo.gametype));
    ("map", `String(gameinfo.Gameinfo.map));
    ("time", `Int(gameinfo.Gameinfo.time));
    ("game_result", `String(Gameinfo.string_of_game_result(gameinfo.Gameinfo.game_result)));
    ("players", `List(json_of_players gameinfo.Gameinfo.players))
  ])

let rec players_of_json (players: Yojson.Basic.json list): Gameinfo.player list =
  match players with
  | [] -> []
  | `Assoc([
      ("name", `String(nm));
      ("clan", `String(cn));
      ("score", `Int(scr));
      ("team", `String(tm))
    ]) :: rest ->
      let player = {
        Gameinfo.name = nm;
        Gameinfo.clan = cn;
        Gameinfo.score = scr;
        Gameinfo.team = Gameinfo.team_of_string tm
      } in
      player :: (players_of_json rest)
  | json :: rest -> raise (error_ill_formed "player" json)

let gameinfo_of_json (gameinfo: Yojson.Basic.json): Teeworlds_message.message =
  match gameinfo with
  | `Assoc([
      ("gametype", `String(gt));
      ("map", `String(map));
      ("time", `Int(time));
      ("game_result", `String(rslt));
      ("players", `List(plrs))
    ]) -> Teeworlds_message.Gameinfo {
        Gameinfo.gametype = gt;
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

let player_request_of_json: Yojson.Basic.json -> Teeworlds_message.message = function
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
        | something_else -> raise (error_unknown_value "player request type" something_else)
      end
  | something_else -> raise (error_ill_formed "player_request" something_else)

let json_of_teeworlds_message: Teeworlds_message.message -> Yojson.Basic.json = function
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

let teeworlds_message_of_json (json: Yojson.Basic.json): Teeworlds_message.message =
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
  | Teeworlds_message.Error str -> `Assoc([
      ("teeworlds_message_type", `String("Error"));
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
  | `Assoc([
      ("teeworlds_message_type", `String("Error"));
      ("teeworlds_message_content", `String(str));
    ]) -> Teeworlds_message.Error str
  | something_else -> raise (error_ill_formed "server_response" something_else)

