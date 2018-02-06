open Db

type t = {id: int64; name: string; clan_id: int64; ctf_rating: int64; dm_rating: int64; secret_key: string}

let empty () = {
  id = Int64.minus_one; name = "";
  clan_id = Int64.minus_one;
  ctf_rating = Int64.minus_one;
  dm_rating = Int64.minus_one;
  secret_key = ""
}

let rating_of_gametype = function
  | Gameinfo.Rctf -> "ctf_rating"
  | Gameinfo.Rtdm -> "dm_rating"

let of_row_e named_row additional_cols =
  let open Sqlite3.Data in
  let fill_player player column = match column with
    | ("id", INT id) -> {player with id = id}
    | ("name", TEXT name) -> {player with name = name}
    | ("clan_id", INT clan_id) -> {player with clan_id = clan_id}
    | ("ctf_rating", INT rating) -> {player with ctf_rating = rating}
    | ("dm_rating", INT rating) -> {player with dm_rating = rating}
    | ("secret_key", TEXT secret_key) -> {player with secret_key = secret_key}
    | _ -> player in
  let player = db_entity_of_row empty named_row [fill_player; additional_cols] in
  if player.name = "" then
    raise (UnexpectedDbData "Retrieved player doesn't have name!")
  else if player.ctf_rating = Int64.zero then
    raise (UnexpectedDbData "Retrieved player doesn't have ctf_rating!")
  else if player.dm_rating = Int64.zero then
    raise (UnexpectedDbData "Retrieved player doesn't have dm_rating!")
  else
    player

let of_row (named_row: named_row): t =
  of_row_e named_row (fun p c -> p)

let of_rows rows =
  List.map of_row rows

let insert_stmt = "insert into players (name, clan_id, ctf_rating, dm_rating, secret_key) values (?, NULL, 1500, 1500, ?)"

let insert (player: t) =
  let open Sqlite3 in
  let prepared_insert_stmt = prepare_bind_stmt insert_stmt
    [Data.TEXT player.name; Data.TEXT player.secret_key] in
  exec_insert_stmt prepared_insert_stmt

let count_stmt = "select count(*) as rows_count from players"

let count (): int64 =
  let s = prepare_stmt count_stmt in
  count_of_row (Option.get (exec_select_single_row_stmt s))

let select_stmt = "select id, name, clan_id, ctf_rating, dm_rating from players where name = ?"

let select (player_name: string): t option =
  let prepared_select_stmt = prepare_bind_stmt select_stmt [Sqlite3.Data.TEXT player_name] in
  (* We need only a single step since name is a PRIMARY KEY and so no more than
   * one row will be returned under select on name *)
  Option.map of_row (exec_select_single_row_stmt prepared_select_stmt)

let select_by_id_stmt = "select id, name, clan_id, ctf_rating, dm_rating from players where id = ?"

let select_by_id (id: int64): t option =
  let s = prepare_bind_stmt select_by_id_stmt [Sqlite3.Data.INT id] in
  Option.map of_row (exec_select_single_row_stmt s)

let select_with_secret_stmt = "select id, name, clan_id, ctf_rating, dm_rating, secret_key from players where name = ?"

let select_with_secret (player_name: string): t option =
  let fill_secret p c = match c with
  | ("secret_key", Sqlite3.Data.TEXT secret_key) -> {p with secret_key = secret_key}
  | _ -> p in
  let prepared_select_stmt = prepare_bind_stmt select_with_secret_stmt
    [Sqlite3.Data.TEXT player_name] in
  Option.map (fun r -> of_row_e r fill_secret) (exec_select_single_row_stmt prepared_select_stmt)

let select_by_rating_stmt rating = "select id, name, clan_id, ctf_rating, dm_rating from players order by " ^ (rating_of_gametype rating) ^ " DESC limit(?) offset(?)"

let select_by_rating limit offset rating =
  let s = prepare_bind_stmt (select_by_rating_stmt rating) [Sqlite3.Data.INT limit; Sqlite3.Data.INT offset] in
  of_rows (exec_select_stmt s)

let select_by_clan_stmt = "select id, name, clan_id, ctf_rating, dm_rating from players where clan_id = ?"

let select_by_clan clan_id =
  let s = prepare_bind_stmt select_by_clan_stmt [Sqlite3.Data.INT clan_id] in
  of_rows (exec_select_stmt s)

let select_top5_stmt = "select id, name, clan_id, ctf_rating, dm_rating from players order by dm_rating DESC limit(5)"

let select_top5 (): t list =
  let prepared_select_stmt = prepare_stmt select_top5_stmt in
  of_rows (exec_select_stmt prepared_select_stmt)

let select_with_dm_rank_stmt =
  "with this_player as ( " ^
  "    select id, name, clan_id, ctf_rating, dm_rating " ^
  "    from players " ^
  "    where name = ? " ^
  "  ), " ^
  "  rank as ( " ^
  "    select count(*) as rank " ^
  "    from players as higher_players, this_player " ^
  "    where this_player.dm_rating < higher_players.dm_rating " ^
  "  ) " ^
  "select id, name, clan_id, ctf_rating, dm_rating, (rank + 1) as rank from this_player, rank "

let select_with_dm_rank (player_name: string): (t * int64) option =
  let rank = ref Int64.minus_one in
  let fill_rank p c = match c with
  | ("rank", Sqlite3.Data.INT r) -> rank := r; p
  | _ -> p in
  let prepared_stmt = prepare_bind_stmt select_with_dm_rank_stmt [Sqlite3.Data.TEXT player_name] in
  let player_with_rank p =
    let pl =  of_row_e p fill_rank in (* First do mutation of the rank ref *)
    (pl, !rank) in
  Option.map player_with_rank (exec_select_single_row_stmt prepared_stmt)

let update_ctf_rating_stmt = "update players set ctf_rating = ctf_rating + ? where name = ?"

let update_ctf_rating (game_id: int64) (player_name: string) (rating_change: int64): unit =
  let open Sqlite3.Data in
  let s = prepare_bind_stmt update_ctf_rating_stmt [INT rating_change; TEXT player_name] in
  exec_update_stmt s

let update_dm_rating_stmt = "update players set dm_rating = dm_rating + ? where name = ?"

let update_dm_rating (game_id: int64) (player_name: string) (rating_change: int64): unit =
  let open Sqlite3.Data in
  let s = prepare_bind_stmt update_dm_rating_stmt [INT rating_change; TEXT player_name] in
  exec_update_stmt s

let update_clan_stmt = "update players set clan_id = ? where id = ?"

let update_clan player_id clan_id =
  let open Sqlite3.Data in
  let s = prepare_bind_stmt update_clan_stmt [INT clan_id; INT player_id] in
  exec_update_stmt s
