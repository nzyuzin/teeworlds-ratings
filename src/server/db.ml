exception UnexpectedErrorCode of string
exception UnexpectedDbData of string

type player = {name: string; clan: string; rating: int64; secret_key: string}

let player_of_row row =
  match row with
  | [|Sqlite3.Data.TEXT nm; Sqlite3.Data.TEXT cn; Sqlite3.Data.INT rtng|] ->
      {name = nm; clan = cn; rating = rtng; secret_key = ""}
  | anything_else ->
      raise (UnexpectedDbData "Retrieved player row doesn't match the expected pattern!")

let players_of_rows rows =
  List.map player_of_row rows

type clan = {clan_name: string; clan_rating: int64}

let clan_of_row = function
  | [|Sqlite3.Data.TEXT name; Sqlite3.Data.INT rating|] ->
      {clan_name = name; clan_rating = rating}
  | anything_else ->
      raise (UnexpectedDbData "Retrieved player row doesn't match the expected pattern!")

type game = {game_id: int64; gametype: string; map: string; game_time: int64; game_result: string; game_date: string}

let game_of_row = let open Sqlite3.Data in function
  | [|INT id; TEXT gt; TEXT mp; INT gtime; TEXT res; TEXT date|] ->
      {game_id = id; gametype = gt; map = mp; game_time = gtime; game_result = res; game_date = date}
  | anything_else ->
      raise (UnexpectedDbData "Retrieved game row doesn't match the expected pattern!")

let games_of_rows rows =
  List.map game_of_row rows

type game_player = {
  game_id: int64;
  player_id: int64;
  score: int64;
  team: string;
  rating_change: int64;
}

let game_player_of_row = let open Sqlite3.Data in function
  | [|INT game_id; INT player_id; INT score; TEXT team; INT rating_change|] ->
      {game_id = game_id; player_id = player_id; score = score; team = team; rating_change = rating_change}
  | anything_else ->
      raise (UnexpectedDbData "Retrieved game_player row doesn't match the expected pattern!")

let game_players_of_rows rows =
  List.map game_player_of_row rows

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

let prepare_bind_stmt stmt_string vals =
  let stmt = prepare_stmt stmt_string in
  let _ = bind_values stmt vals in
  stmt

let step_until_done (stmt: Sqlite3.stmt): Sqlite3.Data.t array list =
  let rec inner aggregator =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.DONE -> aggregator
    | Sqlite3.Rc.ROW -> inner (aggregator @ [Sqlite3.row_data stmt])
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
