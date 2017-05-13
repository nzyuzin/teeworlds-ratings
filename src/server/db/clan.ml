open Db

type t = {id: int64; name: string; sub_clan_id: int64}

let empty () = {id = Int64.minus_one; name = ""; sub_clan_id = Int64.minus_one}

let of_row_e named_row additional_col =
  let fill_clan clan column = match column with
  | ("id", Sqlite3.Data.INT id) -> {clan with id = id}
  | ("name", Sqlite3.Data.TEXT name) -> {clan with name = name}
  | ("sub_clan_id", Sqlite3.Data.INT sub_clan_id) -> {clan with sub_clan_id = sub_clan_id}
  | _ -> clan in
  db_entity_of_row empty named_row [fill_clan; additional_col]

let of_row named_row =
  of_row_e named_row (fun x y -> x)

let insert_stmt = "insert into clans (name) values (?)"

let insert clan =
  let open Sqlite3.Data in
  let s = prepare_bind_stmt insert_stmt [TEXT clan.name] in
  exec_insert_stmt s

let count_stmt = "select count(*) as rows_count from clans"

let count () =
  let s = prepare_stmt count_stmt in
  count_of_row (Option.get (exec_select_single_row_stmt s))

let select_stmt = "select id, name, sub_clan_id from clans where id = ?"

let select clan_id =
  let s = prepare_bind_stmt select_stmt [Sqlite3.Data.INT clan_id] in
  Option.map of_row (exec_select_single_row_stmt s)

let select_by_name_stmt = "select id, name, sub_clan_id from clans where name = ?"

let select_by_name clan_name =
  let s = prepare_bind_stmt select_by_name_stmt [Sqlite3.Data.TEXT clan_name] in
  Option.map of_row (exec_select_single_row_stmt s)

let select_average_rating_stmt =
  "select cast(avg(players.rating) as int) as average_rating from players where clan_id = ?"

let select_average_rating clan_id =
  let average_rating = ref Int64.zero in
  let fill_rating c col = match col with
  | ("average_rating", Sqlite3.Data.INT ar) -> average_rating := ar; c
  | _ -> c in
  let s = prepare_bind_stmt select_average_rating_stmt [Sqlite3.Data.INT clan_id] in
  let _ = Option.map (fun x -> of_row_e x fill_rating) (exec_select_single_row_stmt s) in
  !average_rating
