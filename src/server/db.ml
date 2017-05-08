exception UnexpectedErrorCode of string
exception UnexpectedDbData of string
exception SqliteError of string

type named_row = (string * Sqlite3.Data.t) array

let db = Global.empty "db"

let open_db db_name =
  Global.set db (Sqlite3.db_open ~mode:`NO_CREATE db_name)

let open_db_read_only db_name =
  Global.set db (Sqlite3.db_open ~mode:`READONLY db_name)

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

let begin_transaction () =
  check_ok (Sqlite3.exec (Global.get db) "BEGIN DEFERRED")

let commit_transaction () =
  check_ok (Sqlite3.exec (Global.get db) "COMMIT")

let rollback_transaction () =
  check_ok (Sqlite3.exec (Global.get db) "ROLLBACK")

let prepare_stmt stmt_string =
  try
    Sqlite3.prepare (Global.get db) stmt_string
  with
  | Sqlite3.Error str ->
      let err_msg = "Error while preparing the query <" ^ stmt_string ^ ">: " ^ str in
      raise (SqliteError err_msg)

let prepare_bind_stmt stmt_string vals =
  try
    let stmt = prepare_stmt stmt_string in
    let bind_fun i value = check_ok (Sqlite3.bind stmt (i + 1) value) in
    let _ = List.iteri bind_fun vals in
    stmt
  with
  | Sqlite3.Error str ->
      let err_msg = "Error while binding the query <" ^ stmt_string ^ ">: " ^ str in
      raise (SqliteError err_msg)

let step_until_done (stmt: Sqlite3.stmt): named_row list =
  let rec inner aggregator =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.DONE -> aggregator
    | Sqlite3.Rc.ROW ->
        let named_row = Array.map2 (fun name el -> (name, el)) (Sqlite3.row_names stmt) (Sqlite3.row_data stmt) in
        inner (aggregator @ [named_row])
    | other_code -> raise (error_unexpected_code other_code) in
  inner []

let exec_select_stmt (stmt: Sqlite3.stmt): named_row list =
  let result = step_until_done stmt in
  let _ = check_ok (Sqlite3.finalize stmt) in
  result

let exec_select_single_row_stmt stmt: named_row option =
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

let count_of_row = function
  | [|("rows_count", Sqlite3.Data.INT count)|] -> count
  | _ -> raise (UnexpectedDbData "Unexpected result of count query")

let db_entity_of_row entity_constructor named_row filler_lambdas =
  let entity = entity_constructor () in
  let apply_lambdas ent col =
    List.fold_left (fun ent1 filler_lambda -> filler_lambda ent1 col) ent filler_lambdas in
  Array.fold_left apply_lambdas entity named_row
