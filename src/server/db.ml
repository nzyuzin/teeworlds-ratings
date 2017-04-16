let db_name = "rctf.db"

let open_db () =
  Sqlite3.db_open db_name

let insert_player_stmt = "insert into players values (?, ?, ?);"

let insert_player (player: Gameinfo.tplayer) =
  let open Sqlite3 in
  let open Gameinfo in
  let db = open_db () in
  let prepared_insert_stmt = prepare db insert_player_stmt in
  let _ = bind prepared_insert_stmt 1 (Data.TEXT player.name) in
  let _ = bind prepared_insert_stmt 2 (Data.TEXT player.clan) in
  let _ = bind prepared_insert_stmt 3 (Data.INT (Int64.of_int 1500)) in
  let _ = step prepared_insert_stmt in
  let _ = db_close db in
  ()
