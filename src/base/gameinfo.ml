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
 * <name: string> <clan: string> <score: int> <team: string> <kills: int> <hammer_kills: int> <gun_kills: int> <shotgun_kills: int> <grenade_kills: int> <rifle_kills: int> <deaths: int> <suicides: int> <flag_grabs: int> <flag_captures: int> <flag_returns: int> <flag_carrier_kills: int>
 * ***
 *
 *)

type team = Red | Blue
type game_result =
  | Winner of team
  | Aborted

type player_stats = {
  hammer_kills: int;
  gun_kills: int;
  shotgun_kills: int;
  grenade_kills: int;
  rifle_kills: int;
  deaths: int;
  suicides: int;
  flag_grabs: int;
  flag_captures: int;
  flag_returns: int;
  flag_carrier_kills: int;
}

type player = {
  name: string;
  score: int;
  team: team;
  stats: player_stats;
}

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
    let sr = Parser.until_space player_line (nm_len + 1) in
    let before_tm_len = String.length sr + nm_len + 2 in
    let tm = Parser.until_space player_line before_tm_len in
    let stats_list = Parser.list_of_ints player_line (before_tm_len + (String.length tm) + 1) in
    begin
      Stream.junk players_lines;
      let stats = {
        hammer_kills = List.nth stats_list 0;
        gun_kills = List.nth stats_list 1;
        shotgun_kills = List.nth stats_list 2;
        grenade_kills = List.nth stats_list 3;
        rifle_kills = List.nth stats_list 4;
        deaths = List.nth stats_list 5;
        suicides = List.nth stats_list 6;
        flag_grabs = List.nth stats_list 7;
        flag_captures = List.nth stats_list 8;
        flag_returns = List.nth stats_list 9;
        flag_carrier_kills = List.nth stats_list 10;
      } in
      let player = {
        name = (Parser.unguard_quotes nm);
        score = int_of_string sr;
        team = team_of_string tm;
        stats = stats;
      } in
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
