exception UnknownExternalRequest of string
exception UnknownDataRequest of string
exception UnknownRegistrationRequest of string

type data_request =
  | Players_by_rating of int * int
  | Player_info of string
  | Clan_info of string

type registration_request =
  | Register of string * string
  | Name_available of string

type external_message =
  | Data_request of data_request
  | Registration_request of registration_request

type data_request_response =
  | Players_by_rating of Db.player list
  | Player_info of Db.player * ((Db.game * int64) list)
  | Clan_info of Db.clan * (Db.player list)

type registration_request_response =
  | Register
  | Name_available of bool

type external_message_response =
  | Data_request_response of data_request_response
  | Registration_request_response of registration_request_response
  | Error of string

let players_by_rating_of_json: Json.t -> data_request = function
  | `Assoc([
      ("limit", `Int(limit));
      ("offset", `Int(offset));
    ]) -> Players_by_rating (limit, offset)
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

let data_request_of_json: Json.t -> data_request = function
  | `Assoc([
      ("data_request_type", `String(data_request_type));
      ("data_request_content", body);
    ]) -> begin match data_request_type with
        | "players_by_rating" -> players_by_rating_of_json body
        | "player_info" -> player_info_of_json body
        | "clan_info" -> clan_info_of_json body
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
      ("clan", `String(clan));
    ]) -> Register (name, clan)
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

let json_of_db_player: Db.player -> Json.t = function
  | {Db.name = nm; Db.clan = cn; Db.rating = rtng; Db.secret_key} ->
      `Assoc([
        ("name", `String(nm));
        ("clan", `String(cn));
        ("rating", `Int(Int64.to_int rtng));
        ("secret_key", `String(secret_key));
      ])

let json_of_db_clan: Db.clan -> Json.t = function
  | {Db.clan_name = nm; Db.clan_rating = cr} ->
      `Assoc([
        ("clan_name", `String(nm));
        ("clan_rating", `Int(Int64.to_int cr));
      ])

let json_of_db_player_game: (Db.game * int64) -> Json.t = let open Db in function
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

let json_of_data_request_response: data_request_response -> Json.t = function
  | Players_by_rating players ->
      `Assoc([
        ("data_request_response_type", `String("players_by_rating"));
        ("data_request_response_content", `List(List.map json_of_db_player players));
      ])
  | Player_info (player, games) ->
      `Assoc([
        ("data_request_response_type", `String("player_info"));
        ("data_request_response_content", `Assoc([
          ("player", json_of_db_player player);
          ("games", `List(List.map json_of_db_player_game games));
        ]));
      ])
  | Clan_info (clan, players) ->
      `Assoc([
        ("data_request_response_type", `String("clan_info"));
        ("data_request_response_content", `Assoc([
          ("clan", json_of_db_clan clan);
          ("players", `List(List.map json_of_db_player players));
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
  | Error str ->
      `Assoc([
        ("external_message_type", `String("error"));
        ("external_message_content", `String(str));
      ])
