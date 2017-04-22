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

let average_rating (players: Db.player list): int64 =
  let ratings = List.map (fun player -> player.Db.rating) players in
  let summ = List.fold_left ( Int64.add ) (Int64.of_int 0) ratings in
  Int64.div summ (Int64.of_int (List.length players))

let calculate_new_rating (player: Gameinfo.player) (game_id: int64) (game_result: Gameinfo.game_result): int64 =
  let reds = Db.select_game_players_by_team game_id Gameinfo.Red in
  let blues = Db.select_game_players_by_team game_id Gameinfo.Blue in
  let red_team_rating = average_rating reds in
  let blue_team_rating = average_rating blues in
  let rating_for_team team result = match team with
  | Gameinfo.Red -> elo red_team_rating blue_team_rating result
  | Gameinfo.Blue -> elo blue_team_rating red_team_rating result in
  match game_result with
  | Gameinfo.Aborted -> rating_for_team player.Gameinfo.team Defeat
  | Gameinfo.Winner winner ->
    if player.Gameinfo.team = winner then
      rating_for_team player.Gameinfo.team Victory
    else
      rating_for_team player.Gameinfo.team Defeat

let update_rating (player: Gameinfo.player) (new_rating: int64) =
  Db.update_rating player.Gameinfo.name new_rating
