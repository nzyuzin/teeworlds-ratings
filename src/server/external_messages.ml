exception UnknownExternalRequest of string
exception UnknownDataRequest of string

type data_request =
  | Players_by_rating of int * int
  | Player_info of string
  | Clan_info of string

type data_request_response =
  | Players_by_rating of Db.player list
  | Player_info of Db.player * (Gameinfo.gameinfo list)
  | Clan_info of Db.clan * (Db.player list)

type external_message =
  | Data_request of data_request
  | Data_request_response of data_request_response

let players_by_rating_of_json: Json.t -> data_request = function
  | `Assoc([
      ("offset", `Int(offset));
      ("limit", `Int(limit));
    ]) -> Players_by_rating (offset, limit)
  | something_else -> raise (Json.error_ill_formed "players_by_rating" something_else)

let player_info_of_json: Json.t -> data_request = function
  | `Assoc([
      ("name", `String(name));
    ]) -> Player_info name
  | something_else -> raise (Json.error_ill_formed "clan_info" something_else)

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

let external_message_of_json: Json.t -> external_message = function
  | `Assoc([
      ("external_message_type", `String(message_type));
      ("external_message_content", rest);
    ]) -> begin match message_type with
        | "data_request" -> Data_request (data_request_of_json rest)
        | something_else -> raise (UnknownExternalRequest something_else)
      end
  | something_else -> raise (Json.error_ill_formed "teeworlds_message" something_else)
