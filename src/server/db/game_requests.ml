open Db

let latest_game_of_row = let open Sqlite3.Data in function
  | [|INT id; TEXT gt; TEXT mp; INT gtime; TEXT res; TEXT date; INT rating_change|] ->
      ({game_id = id; gametype = gt; map = mp; game_time = gtime; game_result = res; game_date = date}, rating_change)
  | anything_else ->
      raise (UnexpectedDbData "Retrieved game row doesn't match the expected pattern!")

let select_game_stmt =
  "select id, gametype, map, game_time, game_result, game_date from games where id = ?"

let select_latest_games_by_player_stmt =
  "select games.id, gametype, map, game_time, case when game_result = team then 'Win' else 'Loss' end as game_result, game_date, rating_change from games, game_players, players where game_id = games.id and player_id = players.id and players.name = ? order by games.game_date DESC limit(?)"

let insert_game_stmt =
  "insert into games (gametype, map, game_time, game_result, game_date) " ^
  "values (?, ?, ?, ?, datetime('now'))"

let update_game_rating_change_stmt = "update game_players set rating_change = ? where game_id = ? and player_id = (select id from players where name = ? limit(1))"

let insert_game_player_stmt =
  "insert into game_players (game_id, player_id, score, team) " ^
  "select ? as game_id, id as player_id, ? as score, ? as team from players where name = ?"

let select_game game_id =
  let s = prepare_bind_stmt select_game_stmt [Sqlite3.Data.INT game_id] in
  match exec_select_single_row_stmt s with
  | None -> None
  | Some row -> Some (Db.game_of_row row)

let select_latest_games_by_player player_name limit =
  let s = prepare_bind_stmt select_latest_games_by_player_stmt
    [Sqlite3.Data.TEXT player_name; Sqlite3.Data.INT (Int64.of_int limit)] in
  List.map latest_game_of_row (exec_select_stmt s)

let insert_game (game: Gameinfo.gameinfo) =
  let open Sqlite3 in
  let prepared_insert_stmt = prepare_stmt insert_game_stmt in
  let game_result = Gameinfo.string_of_game_result game.Gameinfo.game_result in
  let _ = bind_values prepared_insert_stmt [
    Data.TEXT game.Gameinfo.gametype;
    Data.TEXT game.Gameinfo.map;
    Data.INT (Int64.of_int game.Gameinfo.time);
    Data.TEXT game_result
  ] in
  exec_insert_stmt prepared_insert_stmt

let insert_game_player (player: Gameinfo.player) game_id =
  let open Sqlite3 in
  let prepared_insert_stmt = prepare_stmt insert_game_player_stmt in
  let team = Gameinfo.string_of_team player.Gameinfo.team in
  let _ = bind_values prepared_insert_stmt [
    Data.INT game_id;
    Data.INT (Int64.of_int player.Gameinfo.score);
    Data.TEXT team;
    Data.TEXT player.Gameinfo.name
  ] in
  exec_insert_stmt prepared_insert_stmt

let select_game_players_stmt =
  "select " ^
  "  players.name, players.clan, players.rating " ^
  "from " ^
  "  players inner join game_players on players.id = game_players.player_id " ^
  "where " ^
  "  game_players.game_id = ?"

let select_game_players_by_team_stmt = select_game_players_stmt ^ " and team = ?"

let select_game_players (game_id: int64): player list =
  let prepared_stmt = prepare_stmt select_game_players_stmt in
  let _ = bind_values prepared_stmt [Sqlite3.Data.INT game_id] in
  let rows = exec_select_stmt prepared_stmt in
  players_of_rows rows

let select_game_players_by_team (game_id: int64) (team: Gameinfo.team): player list =
  let prepared_stmt = prepare_stmt select_game_players_by_team_stmt in
  let _ = bind_values prepared_stmt [
    Sqlite3.Data.INT game_id;
    Sqlite3.Data.TEXT (Gameinfo.string_of_team team)
  ] in
  let rows = exec_select_stmt prepared_stmt in
  players_of_rows rows

let update_rating_change (game_id: int64) (player_name: string) (rating_change: int64) =
  let open Sqlite3 in
  let prepared_rating_change_update_stmt = prepare_stmt update_game_rating_change_stmt in
  let _ = bind_values prepared_rating_change_update_stmt
    [Data.INT rating_change; Data.INT game_id; Data.TEXT player_name] in
  exec_update_stmt prepared_rating_change_update_stmt
