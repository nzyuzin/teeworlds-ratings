exception UnexpectedErrorCode of string

let db = Global.empty "db"

let open_db db_name =
  Global.set db (Sqlite3.db_open ~mode:`NO_CREATE db_name)

let close_db () =
  match Sqlite3.db_close (Global.get db) with
  | true -> ()
  | false -> raise (Failure "Couldn't close the database!")

let error_unexpected_code code =
  UnexpectedErrorCode ("Unexpected return code from Sqlite3: " ^ (Sqlite3.Rc.to_string code))

let check_ok = function
  | Sqlite3.Rc.OK -> ()
  | code -> raise (error_unexpected_code code)

let check_done = function
  | Sqlite3.Rc.DONE -> ()
  | code -> raise (error_unexpected_code code)

let prepare_stmt stmt_string =
  Sqlite3.prepare (Global.get db) stmt_string

let bind_values stmt vals =
  let bind_fun i value = check_ok (Sqlite3.bind stmt (i + 1) value) in
  List.iteri bind_fun vals

let step_until_done (stmt: Sqlite3.stmt): Sqlite3.Data.t array list =
  let rec inner aggregator =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.DONE -> aggregator
    | Sqlite3.Rc.ROW -> inner ((Sqlite3.row_data stmt) :: aggregator)
    | other_code -> raise (error_unexpected_code other_code) in
  inner []

let exec_select_stmt (stmt: Sqlite3.stmt): Sqlite3.Data.t array list =
  let result = step_until_done stmt in
  let _ = check_ok (Sqlite3.finalize stmt) in
  result

let exec_select_single_row_stmt stmt: Sqlite3.Data.t array option =
  let found_rows = exec_select_stmt stmt in
  if (List.length found_rows) > 1 then
    raise (Failure "More than one row selected")
  else if (List.length found_rows) = 1 then
    Some (List.hd found_rows)
  else
    None

let exec_insert_stmt stmt =
  let _ = check_done (Sqlite3.step stmt) in
  let _ = check_ok (Sqlite3.finalize stmt) in
  Sqlite3.last_insert_rowid (Global.get db)

let exec_update_stmt stmt =
  let _ = check_done (Sqlite3.step stmt) in
  check_ok (Sqlite3.finalize stmt)

(* Players *)

type player = {name: string; clan: string; rating: int64}

let player_of_row row =
  match row with
  | [|Sqlite3.Data.TEXT nm; Sqlite3.Data.TEXT cn; Sqlite3.Data.INT rtng|] ->
      {name = nm; clan = cn; rating = rtng}
  | anything_else ->
      raise (Failure "Retrieved player row doesn't match the expected pattern!")

let players_of_rows rows =
  List.map player_of_row rows

let insert_player_stmt = "insert into players (name, clan, rating) values (?, ?, ?)"

let select_player_stmt = "select name, clan, rating from players where name = ?"

let select_top5_players_stmt = "select name, clan, rating from players order by rating DESC limit(5)"

let select_player_with_rank_stmt =
  "with this_player as ( " ^
  "    select name, clan, rating " ^
  "    from players " ^
  "    where name = ? " ^
  "  ), " ^
  "  rank as ( " ^
  "    select count(*) as rank " ^
  "    from players as higher_players, this_player " ^
  "    where this_player.rating < higher_players.rating " ^
  "  ) " ^
  "select name, clan, rating, (rank + 1) as rank from this_player, rank "

let update_rating_stmt = "update players set rating = ? where name = ?"

let insert_player (player: player) =
  let prepared_insert_stmt = prepare_stmt insert_player_stmt in
  let open Sqlite3 in
  let _ = bind_values prepared_insert_stmt
    [Data.TEXT player.name; Data.TEXT player.clan; Data.INT (Int64.of_int 1500)] in
  exec_insert_stmt prepared_insert_stmt

let select_player (player_name: string): player option =
  let prepared_select_stmt = prepare_stmt select_player_stmt in
  let _ = bind_values prepared_select_stmt [Sqlite3.Data.TEXT player_name] in
  (* We need only a single step since name is a PRIMARY KEY and so no more than
   * one row will be returned under select on name *)
  match exec_select_single_row_stmt prepared_select_stmt with
  | None -> None
  | Some row -> Some (player_of_row row)

let select_top5_players (): player list =
  let prepared_select_stmt = prepare_stmt select_top5_players_stmt in
  players_of_rows (exec_select_stmt prepared_select_stmt)

let select_player_with_rank (player_name: string): (player * int64) option =
  let prepared_stmt = prepare_stmt select_player_with_rank_stmt in
  let _ = bind_values prepared_stmt [Sqlite3.Data.TEXT player_name] in
  match (exec_select_single_row_stmt prepared_stmt) with
  | None -> None
  | Some [|Sqlite3.Data.TEXT nm; Sqlite3.Data.TEXT cn; Sqlite3.Data.INT rtng; Sqlite3.Data.INT rank|] ->
      Some ({name = nm; clan = cn; rating = rtng}, rank)
  | anything_else ->
      raise (Failure "Retrieved player row doesn't match the expected pattern!")

let update_rating (player_name: string) (new_rating: int64): unit =
  let open Sqlite3 in
  let prepared_update_stmt = prepare_stmt update_rating_stmt in
  let _ = bind_values prepared_update_stmt [Data.INT new_rating; Data.TEXT player_name] in
  exec_update_stmt prepared_update_stmt

(* Games *)

let insert_game_stmt =
  "insert into games (gametype, map, game_time, game_result, game_date) " ^
  "values (?, ?, ?, ?, datetime('now'))"

let insert_game_player_stmt =
  "insert into game_players (game_id, player_id, score, team) " ^
  "select ? as game_id, id as player_id, ? as score, ? as team from players where name = ?"

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
