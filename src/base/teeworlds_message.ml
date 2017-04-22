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
 * <callback_address: Network.address>
 * <rest of the player request>
 *
 * Player_rank format
 * <player_name: string>
 *)

exception IllFormattedMessage of string
exception UnknownMessageType of string

type player_request =
  | Player_rank of string

type message =
  | Gameinfo of Gameinfo.gameinfo
  | Player_request of player_request * int * Network.address

let parse_player_rank msg =
  let player_line = Stream.next msg in
  let player_name = Parser.read_quoted_word player_line 0 in
  Player_rank (Parser.unguard_quotes player_name)

let parse_player_request msg =
  let player_request_type_str = Stream.next msg in
  let callback_addr_str = Stream.next msg in
  let callback_addr = Network.address_of_string callback_addr_str in
  let client_id_str = Stream.next msg in
  let client_id = int_of_string client_id_str in
  let parsed_player_request =
    match player_request_type_str with
    | "Player_rank" -> parse_player_rank msg
    | other_type -> raise (UnknownMessageType other_type)
  in
  Player_request (parsed_player_request, client_id, callback_addr)

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
