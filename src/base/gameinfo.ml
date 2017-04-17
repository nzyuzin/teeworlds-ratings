(*
 * Gameinfo format:
 *
 * ***
 * Gametype: <string>
 * Map: <string>
 * Time: <int>
 * Winner: <RED | BLUE : string>
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
type player = { name: string; clan: string; score: int; team: team }

type gameinfo = {
  gametype: string;
  map: string;
  time: int;
  winner: team;
  players: player list;
}

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

let parse_gameinfo (info_lines: string Stream.t): gameinfo =
  let id x = x in
  let gametype_str = Stream.next info_lines in
  let gt = Scanf.sscanf gametype_str "Gametype: %s" id in
  let map_str = Stream.next info_lines in
  let mp = Scanf.sscanf map_str "Map: %s" id in
  let time_str = Stream.next info_lines in
  let time_in_seconds = Scanf.sscanf time_str "Gametime: %s" id in
  let winner_str = Stream.next info_lines in
  let wnr = Scanf.sscanf winner_str "Winner: %s" id in
  let _ = Stream.junk info_lines in (* Skip "Players:" line *)
  let plrs = parse_players info_lines in
  {
    gametype = gt;
    map = mp;
    time = int_of_string time_in_seconds;
    winner = team_of_string wnr;
    players = plrs;
  }
