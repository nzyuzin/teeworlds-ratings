open Db

type t = {
  game_id: int64;
  player_id: int64;
  clan_id: int64;
  score: int64;
  team: string;
  rating_change: int64;
  stats: Player_stats.t;
}

let empty () = {
  game_id = Int64.minus_one;
  player_id = Int64.minus_one;
  clan_id = Int64.minus_one;
  score = Int64.minus_one;
  team = "";
  rating_change = Int64.minus_one;
  stats = Player_stats.empty ();
}

let of_row_e named_row additional_cols = let open Sqlite3.Data in
  let fill_game_player gp col = match col with
  | ("game_id", INT game_id) -> {gp with game_id = game_id}
  | ("player_id", INT player_id) -> {gp with player_id = player_id}
  | ("clan_id", INT clan_id) -> {gp with clan_id = clan_id}
  | ("score", INT score) -> {gp with score = score}
  | ("team", TEXT team) -> {gp with team = team}
  | ("rating_change", INT rating_change) -> {gp with rating_change = rating_change}
  | _ -> gp in
  let game_player = db_entity_of_row empty named_row [fill_game_player; additional_cols] in
  let stats = Player_stats.of_row named_row in
  {game_player with stats = stats}

let of_row named_row =
  let gp = of_row_e named_row (fun gp c -> gp) in
  if gp.game_id = Int64.minus_one then
    raise (UnexpectedDbData "Retrieved game_player doesn't have game_id!")
  else if gp.player_id = Int64.minus_one then
    raise (UnexpectedDbData "Retrieved game_player doesn't have player_id!")
  else
    gp

let of_rows rows =
  List.map of_row rows

let insert_stmt =
  "insert into game_players (game_id, player_id, clan_id, score, team, rating_change, hammer_kills, gun_kills, shotgun_kills, grenade_kills, rifle_kills, deaths, suicides, flag_grabs, flag_captures, flag_returns, flag_carrier_kills) " ^
  "select ? as game_id, id as player_id, clan_id as clan_id, ? as score, ? as team, 0 as rating_change, ? as hammer_kills, ? as gun_kills, ? as shotgun_kills, ? as grenade_kills, ? as rifle_kills, ? as deaths, ? as suicides, ? as flag_grabs, ? as flag_captures, ? as flag_returns, ? as flag_carrier_kills from players where name = ?"

let insert (player: Gameinfo.player) game_id =
  let open Sqlite3.Data in let open Gameinfo in
  let team = string_of_team player.team in
  let prepared_insert_stmt = prepare_bind_stmt insert_stmt [
    INT game_id;
    INT (Int64.of_int player.score);
    TEXT team;
    INT (Int64.of_int player.stats.hammer_kills);
    INT (Int64.of_int player.stats.gun_kills);
    INT (Int64.of_int player.stats.shotgun_kills);
    INT (Int64.of_int player.stats.grenade_kills);
    INT (Int64.of_int player.stats.rifle_kills);
    INT (Int64.of_int player.stats.deaths);
    INT (Int64.of_int player.stats.suicides);
    INT (Int64.of_int player.stats.flag_grabs);
    INT (Int64.of_int player.stats.flag_captures);
    INT (Int64.of_int player.stats.flag_returns);
    INT (Int64.of_int player.stats.flag_carrier_kills);
    TEXT player.name
  ] in
  exec_insert_stmt prepared_insert_stmt

let select_by_game_stmt =
  "select game_id, player_id, clan_id, score, team, rating_change, hammer_kills, gun_kills, shotgun_kills, grenade_kills, rifle_kills, deaths, suicides, flag_grabs, flag_captures, flag_returns, flag_carrier_kills from game_players where game_id = ?"

let select_by_game game_id: t list =
  let s = prepare_bind_stmt select_by_game_stmt [Sqlite3.Data.INT game_id] in
  of_rows (exec_select_stmt s)
