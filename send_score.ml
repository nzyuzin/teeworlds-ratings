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

type tgameinfo = { gametype: string; players: tplayer list; winner: tteam }

let line_stream_of_string string = Stream.of_list (Str.split (Str.regexp "\n") string)

let parse_team team_str = if team_str = "RED" then Red else Blue

let rec parse_players (players_lines: string Stream.t) =
  match Stream.peek players_lines with
  | None -> []
  | Some player_line ->
    let nm = "" in
    let cn = "" in
    let sr = 0 in
    let tm = Red in
    begin
      Stream.junk players_lines;
      {name = nm; clan = cn; score = sr; team = tm} :: (parse_players players_lines)
    end

let parse_gameinfo (info_lines: string Stream.t): tgameinfo =
  let id x = x in
  let gametype_str = Stream.next info_lines in
  let gt = Scanf.sscanf gametype_str "Gametype: %s" id in
  let winner_str = Stream.next info_lines in
  let wnr = Scanf.sscanf winner_str "Winner: %s" id in
  let plrs = parse_players info_lines in
  {gametype = gt; winner = (parse_team wnr); players = plrs}

let json_of_gameinfo (gameinfo: tgameinfo) = Yojson.Basic.from_string ""

let usage = "Usage: " ^ Sys.argv.(0) ^ " scores"

let scores: string ref = ref ""

let _ =
  let _ = Arg.parse [] (fun scores' -> scores := scores') usage in
  let parsed_scores = parse_gameinfo (line_stream_of_string !scores) in
  print_endline (Yojson.Basic.pretty_to_string (json_of_gameinfo parsed_scores))
