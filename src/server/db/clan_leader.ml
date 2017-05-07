open Db

type t = {player_id: int64; clan_id: int64}

let empty () = {player_id = Int64.minus_one; clan_id = Int64.minus_one}

let insert_stmt = "insert into clan_leaders (player_id, clan_id) values (?, ?)"

let insert clan_leader =
  let open Sqlite3.Data in
  let s = prepare_bind_stmt insert_stmt [INT clan_leader.player_id; INT clan_leader.clan_id] in
  exec_insert_stmt s
