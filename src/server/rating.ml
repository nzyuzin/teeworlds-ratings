type game_result = Victory | Defeat

let score_of_game_result = function
  | Victory -> 1.
  | Defeat -> 0.

let elo (rating_a: int64) (rating_b: int64) (game_result: game_result): int64 =
  let a = Int64.to_float rating_a in
  let b = Int64.to_float rating_b in
  let expected_score = 1. /. (1. +. (10. ** ((b -. a) /. 400.))) in
  let k = if a >= 2000. then 10. else 20. in
  Int64.of_float (a +. (k *. ((score_of_game_result game_result) -. expected_score)))

let average_rating (players: Player.t list) (player_rating: Player.t -> int64): int64 =
  let ratings = List.map player_rating players in
  let summ = List.fold_left ( Int64.add ) (Int64.of_int 0) ratings in
  Int64.div summ (Int64.of_int (List.length players))

let average_dm_rating players =
  average_rating players (fun p -> p.Player.dm_rating)

let average_ctf_rating players =
  average_rating players (fun p -> p.Player.ctf_rating)

let calculate_new_rating (player: Gameinfo.player) (game_id: int64) (gameinfo: Gameinfo.t): int64 =
  let reds = Game.select_players_by_team game_id Gameinfo.Red in
  let blues = Game.select_players_by_team game_id Gameinfo.Blue in
  let avg = match gameinfo.Gameinfo.gametype with
  | Gameinfo.Rctf -> average_ctf_rating
  | Gameinfo.Rtdm -> average_dm_rating in
  let red_team_rating = avg reds in
  let blue_team_rating = avg blues in
  let rating_for_team team result = match team with
  | Gameinfo.Red -> elo red_team_rating blue_team_rating result
  | Gameinfo.Blue -> elo blue_team_rating red_team_rating result in
  match gameinfo.Gameinfo.game_result with
  | Gameinfo.Aborted -> rating_for_team player.Gameinfo.team Defeat
  | Gameinfo.Winner winner ->
    if player.Gameinfo.team = winner then
      rating_for_team player.Gameinfo.team Victory
    else
      rating_for_team player.Gameinfo.team Defeat

let update_rating (game_id: int64) (gameinfo: Gameinfo.t) (player_name: string) (rating_change: int64) =
  let _ = match gameinfo.Gameinfo.gametype with
  | Gameinfo.Rctf -> Player.update_ctf_rating game_id player_name rating_change
  | Gameinfo.Rtdm -> Player.update_dm_rating game_id player_name rating_change in
  Game.update_rating_change game_id player_name rating_change
