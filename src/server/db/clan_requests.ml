open Db

let select_clan_stmt = "select clan as name, clan_rating from clans where clan = ?"

let select_clan clan_name =
  let s = prepare_bind_stmt select_clan_stmt [Sqlite3.Data.TEXT clan_name] in
  match exec_select_single_row_stmt s with
  | None -> None
  | Some row -> Some (Db.clan_of_row row)
