open Db

let insert_player_stmt = "insert into players (name, clan, rating, secret_key) values (?, ?, ?, ?)"

let select_player_stmt = "select name, clan, rating from players where name = ?"

let select_player_by_id_stmt = "select name, clan, rating from players where id = ?"

let select_player_with_secret_stmt = "select name, clan, rating, secret_key from players where name = ?"

let select_players_by_rating_stmt = "select name, clan, rating from players order by rating DESC limit(?) offset(?)"

let select_players_by_clan_stmt = "select name, clan, rating from players where clan = ?"

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

let update_rating_stmt = "update players set rating = rating + ? where name = ?"

let player_with_secret_of_row = function
  | [|Sqlite3.Data.TEXT nm; Sqlite3.Data.TEXT cn; Sqlite3.Data.INT rtng; Sqlite3.Data.TEXT skey|] ->
      {name = nm; clan = cn; rating = rtng; secret_key = skey}
  | anything_else ->
      raise (Db.UnexpectedDbData "Retrieved player row doesn't match the expected pattern!")

let insert_player (player: player) =
  let prepared_insert_stmt = prepare_stmt insert_player_stmt in
  let open Sqlite3 in
  let _ = bind_values prepared_insert_stmt
    [Data.TEXT player.name; Data.TEXT player.clan; Data.INT (Int64.of_int 1500); Data.TEXT player.secret_key] in
  exec_insert_stmt prepared_insert_stmt

let select_player (player_name: string): player option =
  let prepared_select_stmt = prepare_stmt select_player_stmt in
  let _ = bind_values prepared_select_stmt [Sqlite3.Data.TEXT player_name] in
  (* We need only a single step since name is a PRIMARY KEY and so no more than
   * one row will be returned under select on name *)
  match exec_select_single_row_stmt prepared_select_stmt with
  | None -> None
  | Some row -> Some (player_of_row row)

let select_player_by_id (id: int64): player option =
  let s = prepare_bind_stmt select_player_by_id_stmt [Sqlite3.Data.INT id] in
  match exec_select_single_row_stmt s with
  | None -> None
  | Some row -> Some (player_of_row row)

let select_player_with_secret (player_name: string): player option =
  let prepared_select_stmt = prepare_bind_stmt select_player_with_secret_stmt
    [Sqlite3.Data.TEXT player_name] in
  match exec_select_single_row_stmt prepared_select_stmt with
  | None -> None
  | Some row -> Some (player_with_secret_of_row row)

let select_players_by_rating limit offset =
  let s = prepare_bind_stmt select_players_by_rating_stmt [Sqlite3.Data.INT limit; Sqlite3.Data.INT offset] in
  players_of_rows (exec_select_stmt s)

let select_players_by_clan clan_name =
  let s = prepare_bind_stmt select_players_by_clan_stmt [Sqlite3.Data.TEXT clan_name] in
  players_of_rows (exec_select_stmt s)

let select_top5_players (): player list =
  let prepared_select_stmt = prepare_stmt select_top5_players_stmt in
  players_of_rows (exec_select_stmt prepared_select_stmt)

let select_player_with_rank (player_name: string): (player * int64) option =
  let prepared_stmt = prepare_stmt select_player_with_rank_stmt in
  let _ = bind_values prepared_stmt [Sqlite3.Data.TEXT player_name] in
  match (exec_select_single_row_stmt prepared_stmt) with
  | None -> None
  | Some [|Sqlite3.Data.TEXT nm; Sqlite3.Data.TEXT cn; Sqlite3.Data.INT rtng; Sqlite3.Data.INT rank|] ->
      Some ({name = nm; clan = cn; rating = rtng; secret_key = ""}, rank)
  | anything_else ->
      raise (Failure "Retrieved player row doesn't match the expected pattern!")

let update_rating (game_id: int64) (player_name: string) (rating_change: int64): unit =
  let open Sqlite3 in
  let prepared_update_stmt = prepare_stmt update_rating_stmt in
  let _ = bind_values prepared_update_stmt [Data.INT rating_change; Data.TEXT player_name] in
  exec_update_stmt prepared_update_stmt
