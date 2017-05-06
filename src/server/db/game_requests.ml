open Db

let select_game_stmt =
  "select id, gametype, map, game_time, game_result, game_date from games where id = ?"

let select_latest_games_stmt =
  "select id, gametype, map, game_time, game_result, game_date from games order by game_date DESC limit(?) offset(?)"

let select_game_participants_stmt =
  "select game_id, player_id, score, team, rating_change, hammer_kills, gun_kills, shotgun_kills, grenade_kills, rifle_kills, deaths, suicides, flag_grabs, flag_captures, flag_returns, flag_carrier_kills from game_players where game_id = ?"

let select_latest_games_by_player_stmt =
  "select games.id, gametype, map, game_time, case when game_result = team then 'Win' else 'Loss' end as game_result, game_date, rating_change from games, game_players, players where game_id = games.id and player_id = players.id and players.name = ? order by games.game_date DESC limit(?)"

let select_players_of_game_by_team_stmt =
  "select " ^
  "  players.name, players.clan, players.rating " ^
  "from " ^
  "  players inner join game_players on players.id = game_players.player_id " ^
  "where " ^
  "  game_players.game_id = ? and team = ?"

let select_player_stats_stmt =
  "select player_id, count(*) as total_games, sum(hammer_kills) as hammer_kills, sum(gun_kills) as gun_kills, sum(shotgun_kills) as shotgun_kills, sum(grenade_kills) as grenade_kills, sum(rifle_kills) as rifle_kills, sum(deaths) as deaths, sum(suicides) as suicides, sum(flag_grabs) as flag_grabs, sum(flag_captures) as flag_captures, sum(flag_returns) as flag_returns, sum(flag_carrier_kills) as flag_carrier_kills " ^
  "from game_players inner join players on players.id = game_players.player_id " ^
  "where players.name = ?"

let insert_game_stmt =
  "insert into games (gametype, map, game_time, game_result, game_date) " ^
  "values (?, ?, ?, ?, datetime('now'))"

let update_game_rating_change_stmt = "update game_players set rating_change = ? where game_id = ? and player_id = (select id from players where name = ? limit(1))"

let insert_game_player_stmt =
  "insert into game_players (game_id, player_id, score, team, rating_change, hammer_kills, gun_kills, shotgun_kills, grenade_kills, rifle_kills, deaths, suicides, flag_grabs, flag_captures, flag_returns, flag_carrier_kills) " ^
  "select ? as game_id, id as player_id, ? as score, ? as team, 0 as rating_change, ? as hammer_kills, ? as gun_kills, ? as shotgun_kills, ? as grenade_kills, ? as rifle_kills, ? as deaths, ? as suicides, ? as flag_grabs, ? as flag_captures, ? as flag_returns, ? as flag_carrier_kills from players where name = ?"

let select_game game_id =
  let s = prepare_bind_stmt select_game_stmt [Sqlite3.Data.INT game_id] in
  Option.map Db.game_of_row (exec_select_single_row_stmt s)

let select_latest_games limit offset =
  let s = prepare_bind_stmt select_latest_games_stmt
    [Sqlite3.Data.INT limit; Sqlite3.Data.INT offset] in
  List.map game_of_row (exec_select_stmt s)

let select_game_players game_id: Db.game_player list =
  let s = prepare_bind_stmt select_game_participants_stmt [Sqlite3.Data.INT game_id] in
  Db.game_players_of_rows (exec_select_stmt s)

let select_latest_games_by_player player_name limit =
  let rating_change = ref Int64.minus_one in
  let fill_rating_change g c = match c with
  | ("rating_change", Sqlite3.Data.INT rc) -> rating_change := rc; g
  | _ -> g in
  let s = prepare_bind_stmt select_latest_games_by_player_stmt
    [Sqlite3.Data.TEXT player_name; Sqlite3.Data.INT (Int64.of_int limit)] in
  let game_with_rating_change r =
    let game = game_of_row_e r fill_rating_change in (* mutate rating change ref *)
    (game, !rating_change) in
  List.map game_with_rating_change (exec_select_stmt s)

let select_players_of_game_by_team (game_id: int64) (team: Gameinfo.team): player list =
  let prepared_stmt = prepare_stmt select_players_of_game_by_team_stmt in
  let _ = bind_values prepared_stmt [
    Sqlite3.Data.INT game_id;
    Sqlite3.Data.TEXT (Gameinfo.string_of_team team)
  ] in
  let rows = exec_select_stmt prepared_stmt in
  players_of_rows rows

let select_player_stats player_name: (Db.game_player * int64) option =
  let total_games = ref Int64.minus_one in
  let fill_total_games gp c = match c with
  | ("total_games", Sqlite3.Data.INT tg) -> total_games := tg; gp
  | _ -> gp in
  let prepared_stmt = prepare_bind_stmt select_player_stats_stmt [Sqlite3.Data.TEXT player_name] in
  let player_stats_with_total_games r =
    let gp = game_player_of_row_e r fill_total_games in
    (gp, !total_games) in
  Option.map player_stats_with_total_games (exec_select_single_row_stmt prepared_stmt)

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
  let open Sqlite3.Data in let open Gameinfo in
  let team = string_of_team player.team in
  let prepared_insert_stmt = prepare_bind_stmt insert_game_player_stmt [
    INT game_id;
    INT (Int64.of_int player.score);
    TEXT team;
    INT (Int64.of_int player.stats.hammer_kills);
    INT (Int64.of_int player.stats.gun_kills);
    INT (Int64.of_int player.stats.shotgun_kills);
    INT (Int64.of_int player.stats.grenade_kills);
    INT (Int64.of_int player.stats.rifle_kills);
    INT (Int64.of_int player.stats.deaths);
    INT (Int64.of_int player.stats.suicides);
    INT (Int64.of_int player.stats.flag_grabs);
    INT (Int64.of_int player.stats.flag_captures);
    INT (Int64.of_int player.stats.flag_returns);
    INT (Int64.of_int player.stats.flag_carrier_kills);
    TEXT player.name
  ] in
  exec_insert_stmt prepared_insert_stmt

let update_rating_change (game_id: int64) (player_name: string) (rating_change: int64) =
  let open Sqlite3 in
  let prepared_rating_change_update_stmt = prepare_stmt update_game_rating_change_stmt in
  let _ = bind_values prepared_rating_change_update_stmt
    [Data.INT rating_change; Data.INT game_id; Data.TEXT player_name] in
  exec_update_stmt prepared_rating_change_update_stmt
