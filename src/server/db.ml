type player = {name: string; clan: string; rating: int64}

let db_name = "rctf.db"

let open_db () =
  Sqlite3.db_open db_name

let insert_player_stmt = "insert into players values (?, ?, ?)"

let select_player_stmt = "select name, clan, score from players where name = ?"

let update_rating_stmt = "update players set rating = ? where name = ?"

let insert_player (player: player) =
  let open Sqlite3 in
  let db = open_db () in
  let prepared_insert_stmt = prepare db insert_player_stmt in
  let _ = bind prepared_insert_stmt 1 (Data.TEXT player.name) in
  let _ = bind prepared_insert_stmt 2 (Data.TEXT player.clan) in
  let _ = bind prepared_insert_stmt 3 (Data.INT (Int64.of_int 1500)) in
  let _ = step prepared_insert_stmt in
  let _ = db_close db in
  ()

let select_player (player_name: string): player option =
  let open Sqlite3 in
  let db = open_db () in
  let prepared_select_stmt = prepare db select_player_stmt in
  let _ = bind prepared_select_stmt 1 (Data.TEXT player_name) in
  let _ = step prepared_select_stmt in
  let data = row_data prepared_select_stmt in
  match data with
  | [| |] -> None
  | [|Data.TEXT nm; Data.TEXT cn; Data.INT rtng|] ->
      Some {name = nm; clan = cn; rating = rtng}
  | anything_else -> raise (Failure "Retrieved player row doesn't match the expected pattern!")

let update_rating (player_name: string) (new_rating: int64): unit =
  let open Sqlite3 in
  let db = open_db () in
  let prepared_update_stmt = prepare db update_rating_stmt in
  let _ = bind prepared_update_stmt 1 (Data.INT new_rating) in
  let _ = bind prepared_update_stmt 2 (Data.TEXT player_name) in
  let _ = step prepared_update_stmt in
  let _ = db_close db in
  ()
