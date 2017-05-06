exception UnexpectedErrorCode of string
exception UnexpectedDbData of string

let count_of_row = function
  | [|("rows_count", Sqlite3.Data.INT count)|] -> count
  | _ -> raise (UnexpectedDbData "Unexpected result of count query")

type named_row = (string * Sqlite3.Data.t) array

let db_entity_of_row entity_constructor named_row filler_lambdas =
  let entity = entity_constructor () in
  let apply_lambdas ent col =
    List.fold_left (fun ent1 filler_lambda -> filler_lambda ent1 col) ent filler_lambdas in
  Array.fold_left apply_lambdas entity named_row

type player = {id: int64; name: string; clan: string; rating: int64; secret_key: string}

let empty_player () = {id = Int64.minus_one; name = ""; clan = ""; rating = Int64.minus_one; secret_key = ""}

let player_of_row_e named_row additional_cols =
  let open Sqlite3.Data in
  let fill_player player column = match column with
    | ("id", INT id) -> {player with id = id}
    | ("name", TEXT name) -> {player with name = name}
    | ("clan", TEXT clan) -> {player with clan = clan}
    | ("rating", INT rating) -> {player with rating = rating}
    | ("secret_key", TEXT secret_key) -> {player with secret_key = secret_key}
    | _ -> player in
  let player = db_entity_of_row empty_player named_row [fill_player; additional_cols] in
  if player.name = "" then
    raise (UnexpectedDbData "Retrieved player doesn't have name!")
  else if player.rating = Int64.zero then
    raise (UnexpectedDbData "Retrieved player doesn't have rating!")
  else
    player

let player_of_row (named_row: named_row): player =
  player_of_row_e named_row (fun p c -> p)

let players_of_rows rows =
  List.map player_of_row rows

type clan = {clan_name: string; clan_rating: int64}

let empty_clan () = {clan_name = ""; clan_rating = Int64.minus_one}

let clan_of_row named_row =
  let fill_clan clan column = match column with
  | ("name", Sqlite3.Data.TEXT name) -> {clan with clan_name = name}
  | ("clan_rating", Sqlite3.Data.INT rating) -> {clan with clan_rating = rating}
  | _ -> clan in
  db_entity_of_row empty_clan named_row [fill_clan]

type game = {game_id: int64; gametype: string; map: string; game_time: int64; game_result: string; game_date: string}

let empty_game () =
  {game_id = Int64.minus_one; gametype = ""; map = ""; game_time = Int64.minus_one; game_result = ""; game_date = ""}

let game_of_row_e named_row additional_cols = let open Sqlite3.Data in
  let fill_game game column = match column with
  | ("id", INT game_id) -> {game with game_id = game_id}
  | ("gametype", TEXT gametype) -> {game with gametype = gametype}
  | ("map", TEXT map) -> {game with map = map}
  | ("game_time", INT game_time) -> {game with game_time = game_time}
  | ("game_result", TEXT game_result) -> {game with game_result = game_result}
  | ("game_date", TEXT game_date) -> {game with game_date = game_date}
  | _ -> game in
  let game = db_entity_of_row empty_game named_row [fill_game; additional_cols] in
  if game.game_id = Int64.minus_one then
    raise (UnexpectedDbData "Retrieved game doesn't have id!")
  else
    game

let game_of_row named_row =
  game_of_row_e named_row (fun g c -> g)

let games_of_rows rows =
  List.map game_of_row rows

type player_stats = {
  hammer_kills: int64;
  gun_kills: int64;
  shotgun_kills: int64;
  grenade_kills: int64;
  rifle_kills: int64;
  deaths: int64;
  suicides: int64;
  flag_grabs: int64;
  flag_captures: int64;
  flag_returns: int64;
  flag_carrier_kills: int64;
}

let empty_stats () = {
  hammer_kills = Int64.minus_one;
  gun_kills = Int64.minus_one;
  shotgun_kills = Int64.minus_one;
  grenade_kills = Int64.minus_one;
  rifle_kills = Int64.minus_one;
  deaths = Int64.minus_one;
  suicides = Int64.minus_one;
  flag_grabs = Int64.minus_one;
  flag_captures = Int64.minus_one;
  flag_returns = Int64.minus_one;
  flag_carrier_kills = Int64.minus_one;
}

type game_player = {
  game_id: int64;
  player_id: int64;
  score: int64;
  team: string;
  rating_change: int64;
  stats: player_stats;
}

let empty_game_player () = {
  game_id = Int64.minus_one;
  player_id = Int64.minus_one;
  score = Int64.minus_one;
  team = "";
  rating_change = Int64.minus_one;
  stats = empty_stats ();
}

let game_player_of_row_e named_row additional_cols = let open Sqlite3.Data in
  let fill_game_player gp col = match col with
  | ("game_id", INT game_id) -> {gp with game_id = game_id}
  | ("player_id", INT player_id) -> {gp with player_id = player_id}
  | ("score", INT score) -> {gp with score = score}
  | ("team", TEXT team) -> {gp with team = team}
  | ("rating_change", INT rating_change) -> {gp with rating_change = rating_change}
  | ("hammer_kills", INT hammer_kills) -> {gp with stats = {gp.stats with hammer_kills = hammer_kills}}
  | ("gun_kills", INT gun_kills) -> {gp with stats = {gp.stats with gun_kills = gun_kills}}
  | ("shotgun_kills", INT shotgun_kills) -> {gp with stats = {gp.stats with shotgun_kills = shotgun_kills}}
  | ("grenade_kills", INT grenade_kills) -> {gp with stats = {gp.stats with grenade_kills = grenade_kills}}
  | ("rifle_kills", INT rifle_kills) -> {gp with stats = {gp.stats with rifle_kills = rifle_kills}}
  | ("deaths", INT deaths) -> {gp with stats = {gp.stats with deaths = deaths}}
  | ("suicides", INT suicides) -> {gp with stats = {gp.stats with suicides = suicides}}
  | ("flag_grabs", INT flag_grabs) -> {gp with stats = {gp.stats with flag_grabs = flag_grabs}}
  | ("flag_captures", INT flag_captures) -> {gp with stats = {gp.stats with flag_captures = flag_captures}}
  | ("flag_returns", INT flag_returns) -> {gp with stats = {gp.stats with flag_returns = flag_returns}}
  | ("flag_carrier_kills", INT flag_carrier_kills) -> {gp with stats = {gp.stats with flag_carrier_kills = flag_carrier_kills}}
  | _ -> gp in
  db_entity_of_row empty_game_player named_row [fill_game_player; additional_cols]

let game_player_of_row named_row =
  let gp = game_player_of_row_e named_row (fun gp c -> gp) in
  if gp.game_id = Int64.minus_one then
    raise (UnexpectedDbData "Retrieved game_player doesn't have game_id!")
  else if gp.player_id = Int64.minus_one then
    raise (UnexpectedDbData "Retrieved game_player doesn't have player_id!")
  else
    gp

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
