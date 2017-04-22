(*
 * Gameinfo format:
 *
 * ***
 * Gametype: <string>
 * Map: <string>
 * Gametime: <int>
 * Result: <game_result>
 * Players:
 * <player list, each player on new line>
 * ***
 *
 * Player format:
 * ***
 * <name> <clan> <score> <team>
 * ***
 *
 *)

type team = Red | Blue
type game_result =
  | Winner of team
  | Aborted
type player = { name: string; clan: string; score: int; team: team }

type gameinfo = {
  gametype: string;
  map: string;
  time: int;
  game_result: game_result;
  players: player list;
}

let team_of_string team_str = if team_str = "RED" then Red else Blue

let string_of_team = function
  | Red -> "RED"
  | Blue -> "BLUE"

let opposite_team = function
  | Red -> Blue
  | Blue -> Red

let game_result_of_string str = Winner (team_of_string str)

let string_of_game_result = function
  | Aborted -> "ABORTED"
  | Winner tm -> string_of_team tm

let rec parse_players (players_lines: string Stream.t) =
  match Stream.peek players_lines with
  | None -> []
  | Some player_line ->
    let nm = Parser.read_quoted_word player_line 0 in
    let nm_len = String.length nm + 2 in
    let cn = Parser.read_quoted_word player_line (nm_len + 1) in
    let cn_len = String.length cn + 2 in
    let sr = Parser.until_space player_line (nm_len + cn_len + 2) in
    let before_tm_len = String.length sr + cn_len + nm_len + 3 in
    let tm = String.sub player_line before_tm_len (String.length player_line - before_tm_len) in
    begin
      Stream.junk players_lines;
      let player = {name = (Parser.unguard_quotes nm);
                    clan = (Parser.unguard_quotes cn);
                    score = int_of_string sr;
                    team = team_of_string tm} in
        player :: (parse_players players_lines)
    end

let parse_gameinfo (info_lines: string Stream.t): gameinfo =
  let id x = x in
  let gametype_str = Stream.next info_lines in
  let gt = Scanf.sscanf gametype_str "Gametype: %s" id in
  let map_str = Stream.next info_lines in
  let mp = Scanf.sscanf map_str "Map: %s" id in
  let time_str = Stream.next info_lines in
  let time_in_seconds = Scanf.sscanf time_str "Gametime: %s" id in
  let game_result_str = Stream.next info_lines in
  let gmrslt = Scanf.sscanf game_result_str "Result: %s" id in
  let _ = Stream.junk info_lines in (* Skip "Players:" line *)
  let plrs = parse_players info_lines in
  {
    gametype = gt;
    map = mp;
    time = int_of_string time_in_seconds;
    game_result = game_result_of_string gmrslt;
    players = plrs;
  }
