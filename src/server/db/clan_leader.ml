open Db

type t = {player_id: int64; clan_id: int64}

let empty () = {player_id = Int64.minus_one; clan_id = Int64.minus_one}

let of_row named_row =
  let fill_clan_leader cl c = match c with
  | ("player_id", Sqlite3.Data.INT player_id) -> {cl with player_id = player_id}
  | ("clan_id", Sqlite3.Data.INT clan_id) -> {cl with clan_id = clan_id}
  | _ -> cl in
  db_entity_of_row empty named_row [fill_clan_leader]

let insert_stmt = "insert into clan_leaders (player_id, clan_id) values (?, ?)"

let insert clan_leader =
  let open Sqlite3.Data in
  let s = prepare_bind_stmt insert_stmt [INT clan_leader.player_id; INT clan_leader.clan_id] in
  exec_insert_stmt s

let select_by_clan_stmt = "select player_id, clan_id from clan_leaders where clan_id = ?"

let select_by_clan (clan_id: int64): t option =
  let s = prepare_bind_stmt select_by_clan_stmt [Sqlite3.Data.INT clan_id] in
  Option.map of_row (exec_select_single_row_stmt s)
