let db = Global.empty "db"

let open_db db_name =
  Global.set db (Sqlite3.db_open ~mode:`NO_CREATE db_name)

let close_db () =
  match Sqlite3.db_close (Global.get db) with
  | true -> ()
  | false -> raise (Failure "Couldn't close the database!")

(* Players *)

type player = {name: string; clan: string; rating: int64}

let insert_player_stmt = "insert into players (name, clan, rating) values (?, ?, ?)"

let select_player_stmt = "select name, clan, rating from players where name = ?"

let update_rating_stmt = "update players set rating = ? where name = ?"

let insert_player (player: player) =
  let open Sqlite3 in
  let prepared_insert_stmt = prepare (Global.get db) insert_player_stmt in
  let _ = bind prepared_insert_stmt 1 (Data.TEXT player.name) in
  let _ = bind prepared_insert_stmt 2 (Data.TEXT player.clan) in
  let _ = bind prepared_insert_stmt 3 (Data.INT (Int64.of_int 1500)) in
  let _ = step prepared_insert_stmt in
  let _ = finalize prepared_insert_stmt in
  last_insert_rowid (Global.get db)

let select_player (player_name: string): player option =
  let open Sqlite3 in
  let prepared_select_stmt = prepare (Global.get db) select_player_stmt in
  let _ = bind prepared_select_stmt 1 (Data.TEXT player_name) in
  (* We need only a single step since name is a PRIMARY KEY and so no more than
   * one row will be returned under select on name *)
  let _ = step prepared_select_stmt in
  let data = row_data prepared_select_stmt in
  let _ = finalize prepared_select_stmt in
  match data with
  | [| |] -> None
  | [|Data.TEXT nm; Data.TEXT cn; Data.INT rtng|] ->
      Some {name = nm; clan = cn; rating = rtng}
  | anything_else ->
      raise (Failure "Retrieved player row doesn't match the expected pattern!")

let update_rating (player_name: string) (new_rating: int64): unit =
  let open Sqlite3 in
  let prepared_update_stmt = prepare (Global.get db) update_rating_stmt in
  let _ = bind prepared_update_stmt 1 (Data.INT new_rating) in
  let _ = bind prepared_update_stmt 2 (Data.TEXT player_name) in
  let _ = step prepared_update_stmt in
  let _ = finalize prepared_update_stmt in
  ()

(* Games *)

let insert_game_stmt =
  "insert into games (gametype, map, game_time, winner, game_date) " ^
  "values (?, ?, ?, ?, datetime('now'))"

let insert_game_player_stmt =
  "insert into game_players (game_id, player_id, score, team) " ^
  "select ? as game_id, id as player_id, ? as score, ? as team from players where name = ?"

let insert_game (game: Gameinfo.gameinfo) =
  let open Sqlite3 in
  let prepared_insert_stmt = prepare (Global.get db) insert_game_stmt in
  let _ = bind prepared_insert_stmt 1 (Data.TEXT game.Gameinfo.gametype) in
  let _ = bind prepared_insert_stmt 2 (Data.TEXT game.Gameinfo.map) in
  let _ = bind prepared_insert_stmt 3 (Data.INT (Int64.of_int game.Gameinfo.time)) in
  let winner = Gameinfo.string_of_team game.Gameinfo.winner in
  let _ = bind prepared_insert_stmt 4 (Data.TEXT winner) in
  let _ = step prepared_insert_stmt in
  let _ = finalize prepared_insert_stmt in
  last_insert_rowid (Global.get db)

let insert_game_player (player: Gameinfo.player) game_id =
  let open Sqlite3 in
  let prepared_insert_stmt = prepare (Global.get db) insert_game_player_stmt in
  let _ = bind prepared_insert_stmt 1 (Data.INT game_id) in
  let _ = bind prepared_insert_stmt 2 (Data.INT (Int64.of_int player.Gameinfo.score)) in
  let team = Gameinfo.string_of_team player.Gameinfo.team in
  let _ = bind prepared_insert_stmt 3 (Data.TEXT team) in
  let _ = bind prepared_insert_stmt 4 (Data.TEXT player.Gameinfo.name) in
  let _ = step prepared_insert_stmt in
  let _ = finalize prepared_insert_stmt in
  last_insert_rowid (Global.get db)
