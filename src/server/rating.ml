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
  List.fold_left ( Int64.add ) (Int64.of_int 0) ratings

let update_rating (player: Gameinfo.player) (game_id: int64) (winner: Gameinfo.team): unit =
  let winners = Db.select_game_players_by_team game_id winner in
  let losers = Db.select_game_players_by_team game_id (Gameinfo.opposite_team winner) in
  let winning_team_rating = average_rating winners in
  let losing_team_rating = average_rating losers in
  let new_rating = if player.Gameinfo.team = winner then
      elo winning_team_rating losing_team_rating Victory
    else
      elo losing_team_rating winning_team_rating Defeat in
  Db.update_rating player.Gameinfo.name new_rating
