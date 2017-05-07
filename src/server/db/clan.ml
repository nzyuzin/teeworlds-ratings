open Db

type t = {name: string; rating: int64}

let empty () = {name = ""; rating = Int64.minus_one}

let of_row named_row =
  let fill_clan clan column = match column with
  | ("name", Sqlite3.Data.TEXT name) -> {clan with name = name}
  | ("clan_rating", Sqlite3.Data.INT rating) -> {clan with rating = rating}
  | _ -> clan in
  db_entity_of_row empty named_row [fill_clan]

let select_stmt = "select clan as name, clan_rating from clans where clan = ?"

let select clan_name =
  let s = prepare_bind_stmt select_stmt [Sqlite3.Data.TEXT clan_name] in
  Option.map of_row (exec_select_single_row_stmt s)
