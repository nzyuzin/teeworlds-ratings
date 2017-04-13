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
  match Stream.peek players_lines with
  | None -> []
  | Some player_line ->
    let nm = until_space player_line 0 in
    let nm_len = String.length nm in
    let cn = until_space player_line (nm_len + 1) in
    let cn_len = String.length cn in
    let sr = until_space player_line (nm_len + cn_len + 2) in
    let before_tm_len = String.length sr + cn_len + nm_len + 3 in
    let tm = String.sub player_line before_tm_len (String.length player_line - before_tm_len) in
    begin
      Stream.junk players_lines;
      {name = nm; clan = cn; score = int_of_string sr; team = team_of_string tm} 
        :: (parse_players players_lines)
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

let _ =
  let _ = Arg.parse [] (fun scores' -> scores := scores') usage in
  let parsed_scores = parse_gameinfo (line_stream_of_string !scores) in
  print_endline (Yojson.Basic.pretty_to_string (json_of_gameinfo parsed_scores))
