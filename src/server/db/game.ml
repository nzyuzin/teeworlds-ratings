open Db

type t = {game_id: int64; gametype: string; map: string; game_time: int64; game_result: string; game_date: string}

let empty () =
  {game_id = Int64.minus_one; gametype = ""; map = ""; game_time = Int64.minus_one; game_result = ""; game_date = ""}

let of_row_e named_row additional_cols = let open Sqlite3.Data in
  let fill_game game column = match column with
  | ("id", INT game_id) -> {game with game_id = game_id}
  | ("gametype", TEXT gametype) -> {game with gametype = gametype}
  | ("map", TEXT map) -> {game with map = map}
  | ("game_time", INT game_time) -> {game with game_time = game_time}
  | ("game_result", TEXT game_result) -> {game with game_result = game_result}
  | ("game_date", TEXT game_date) -> {game with game_date = game_date}
  | _ -> game in
  let game = db_entity_of_row empty named_row [fill_game; additional_cols] in
  if game.game_id = Int64.minus_one then
    raise (UnexpectedDbData "Retrieved game doesn't have id!")
  else
    game

let of_row named_row =
  of_row_e named_row (fun g c -> g)

let of_rows rows =
  List.map of_row rows

let insert_stmt =
  "insert into games (gametype, map, game_time, game_result, game_date) " ^
  "values (?, ?, ?, ?, datetime('now'))"

let insert (game: Gameinfo.t) =
  let open Sqlite3 in
  let game_result = Gameinfo.string_of_game_result game.Gameinfo.game_result in
  let prepared_insert_stmt = prepare_bind_stmt insert_stmt [
    Data.TEXT (Gameinfo.string_of_gametype game.Gameinfo.gametype);
    Data.TEXT game.Gameinfo.map;
    Data.INT (Int64.of_int game.Gameinfo.time);
    Data.TEXT game_result
  ] in
  exec_insert_stmt prepared_insert_stmt

let count_stmt =
  "select count(*) as rows_count from games"

let count () =
  let s = prepare_stmt count_stmt in
  count_of_row (Option.get (exec_select_single_row_stmt s))

let select_stmt =
  "select id, gametype, map, game_time, game_result, game_date from games where id = ?"

let select game_id =
  let s = prepare_bind_stmt select_stmt [Sqlite3.Data.INT game_id] in
  Option.map of_row (exec_select_single_row_stmt s)

let select_latest_stmt =
  "select id, gametype, map, game_time, game_result, game_date from games order by game_date DESC limit(?) offset(?)"

let select_latest limit offset =
  let s = prepare_bind_stmt select_latest_stmt
    [Sqlite3.Data.INT limit; Sqlite3.Data.INT offset] in
  List.map of_row (exec_select_stmt s)

let select_latest_by_player_stmt =
  "select games.id, gametype, map, game_time, case when game_result = team then 'Win' else 'Loss' end as game_result, game_date, rating_change from games, game_players, players where game_id = games.id and player_id = players.id and players.name = ? order by games.game_date DESC limit(?)"

let select_latest_by_player player_name limit: (t * int64) list =
  let rating_change = ref Int64.minus_one in
  let fill_rating_change g c = match c with
  | ("rating_change", Sqlite3.Data.INT rc) -> rating_change := rc; g
  | _ -> g in
  let s = prepare_bind_stmt select_latest_by_player_stmt
    [Sqlite3.Data.TEXT player_name; Sqlite3.Data.INT (Int64.of_int limit)] in
  let game_with_rating_change r =
    let game = of_row_e r fill_rating_change in (* mutate rating change ref *)
    (game, !rating_change) in
  List.map game_with_rating_change (exec_select_stmt s)

let select_players_by_team_stmt =
  "select " ^
  "  players.name, players.clan_id, players.rating " ^
  "from " ^
  "  players inner join game_players on players.id = game_players.player_id " ^
  "where " ^
  "  game_players.game_id = ? and team = ?"

let select_players_by_team (game_id: int64) (team: Gameinfo.team): Player.t list =
  let prepared_stmt = prepare_bind_stmt select_players_by_team_stmt [
    Sqlite3.Data.INT game_id;
    Sqlite3.Data.TEXT (Gameinfo.string_of_team team)
  ] in
  let rows = exec_select_stmt prepared_stmt in
  Player.of_rows rows

let update_rating_change_stmt = "update game_players set rating_change = ? where game_id = ? and player_id = (select id from players where name = ? limit(1))"

let update_rating_change (game_id: int64) (player_name: string) (rating_change: int64) =
  let open Sqlite3 in
  let s = prepare_bind_stmt update_rating_change_stmt
    [Data.INT rating_change; Data.INT game_id; Data.TEXT player_name] in
  exec_update_stmt s
