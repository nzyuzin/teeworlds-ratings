(*
 * Gameinfo format:
 *
 * ***
 * Gametype: <type>
 * Winner: <team>
 * Players:
 * <players>
 * ***
 *
 * Player format:
 * ***
 * <name> <clan> <score> <team>
 * ***
 *
 *)

let debug: bool ref = ref false

type tteam = Red | Blue
type tplayer = { name: string; clan: string; score: int; team: tteam }

type tgameinfo = { gametype: string; winner: tteam; players: tplayer list; }

let line_stream_of_string string = Stream.of_list (Str.split (Str.regexp "\n") string)

let team_of_string team_str = if team_str = "RED" then Red else Blue

let string_of_team = function
  | Red -> "RED"
  | Blue -> "BLUE"

let rec parse_players (players_lines: string Stream.t) =
  let until_char str chr from_pos =
    String.sub str from_pos ((String.index_from str from_pos chr) - from_pos) in
  let until_space str from_pos =
    until_char str ' ' from_pos in
  let read_quoted_word str start_pos =
    let rec find_matching_quote pos =
      let first_quote = String.index_from str pos '"' in
      if str.[first_quote - 1] = '\\' then
        find_matching_quote (first_quote + 1)
      else
        first_quote in
    if str.[start_pos] != '"' then
      raise (Failure ("No quotation at '" ^ str ^ "'!"))
    else
      let end_quote = find_matching_quote (start_pos + 1) in
      String.sub str (start_pos + 1) (end_quote - start_pos - 1) in
  let unguard_quotes str =
    Str.global_replace (Str.regexp_string "\\\"") "\"" str in
  match Stream.peek players_lines with
  | None -> []
  | Some player_line ->
    let nm = read_quoted_word player_line 0 in
    let nm_len = String.length nm + 2 in
    let cn = read_quoted_word player_line (nm_len + 1) in
    let cn_len = String.length cn + 2 in
    let sr = until_space player_line (nm_len + cn_len + 2) in
    let before_tm_len = String.length sr + cn_len + nm_len + 3 in
    let tm = String.sub player_line before_tm_len (String.length player_line - before_tm_len) in
    begin
      Stream.junk players_lines;
      let player = {name = (unguard_quotes nm); 
                    clan = (unguard_quotes cn); 
                    score = int_of_string sr; 
                    team = team_of_string tm} in
        player :: (parse_players players_lines)
    end

let parse_gameinfo (info_lines: string Stream.t): tgameinfo =
  let id x = x in
  let gametype_str = Stream.next info_lines in
  let gt = Scanf.sscanf gametype_str "Gametype: %s" id in
  let winner_str = Stream.next info_lines in
  let wnr = Scanf.sscanf winner_str "Winner: %s" id in
  let _ = Stream.junk info_lines in (* Skip "Players:" line *)
  let plrs = parse_players info_lines in
  {gametype = gt; winner = (team_of_string wnr); players = plrs}

let rec json_of_players (players: tplayer list) = match players with
  | [] -> []
  | player :: rest -> let open Yojson.Basic in
    `Assoc([
      ("name", `String(player.name));
      ("clan", `String(player.clan));
      ("score", `Int(player.score));
      ("team", `String(string_of_team(player.team)))
    ]) :: (json_of_players rest)

let json_of_gameinfo (gameinfo: tgameinfo) =
  let open Yojson.Basic in
  `Assoc([
    ("gametype", `String(gameinfo.gametype));
    ("winner", `String(string_of_team(gameinfo.winner)));
    ("players", `List(json_of_players gameinfo.players))
  ])

let usage = "Usage: " ^ Sys.argv.(0) ^ " scores"

let scores: string ref = ref ""
let server_ip: string ref = ref ""
let server_port: int ref = ref (-1)

let cl_arguments = [
  ("-d", Arg.Set(debug), "Enables debug logging");
  ("-s", Arg.Set_string(server_ip), "IP to which the scores will be sent");
  ("-p", Arg.Set_int(server_port), "Port to which the scores will be sent");
]

type connection = in_channel * out_channel

let in_connection ((i, o): connection) = i
let out_connection ((i, o): connection) = o

let create_connection (ip: string) (port: int): connection =
  let addr = Unix.ADDR_INET (Unix.inet_addr_of_string ip, port) in
  Unix.open_connection addr

let close_connection (conn: connection): unit =
  let in_conn = in_connection conn in
  let _ = Unix.shutdown_connection in_conn in
  close_in in_conn

let prdebug msg = if !debug then prerr_endline msg

let _ =
  let _ = Arg.parse cl_arguments (fun scores' -> scores := scores') usage in
  let _ = prdebug ("Input:\n" ^ !scores ^ "\n") in
  let parsed_scores = parse_gameinfo (line_stream_of_string !scores) in
  let jsoned_scores = json_of_gameinfo parsed_scores in
  let _ = prdebug ("Output:\n" ^ (Yojson.Basic.pretty_to_string jsoned_scores) ^ "\n") in
  let _ = prdebug ("Sending message to " ^ !server_ip ^ ":" ^ (string_of_int !server_port)) in
  let conn = create_connection !server_ip !server_port in
  let out_conn = out_connection conn in
  let _ = Yojson.Basic.to_channel out_conn jsoned_scores in
  let _ = flush out_conn in
  close_connection conn

