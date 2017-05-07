open Db

type t = {
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

let empty () = {
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

let of_row_e named_row additional_cols = let open Sqlite3.Data in
  let fill_player_stats ps col = match col with
  | ("hammer_kills", INT hammer_kills) -> {ps with hammer_kills = hammer_kills}
  | ("gun_kills", INT gun_kills) -> {ps with gun_kills = gun_kills}
  | ("shotgun_kills", INT shotgun_kills) -> {ps with shotgun_kills = shotgun_kills}
  | ("grenade_kills", INT grenade_kills) -> {ps with grenade_kills = grenade_kills}
  | ("rifle_kills", INT rifle_kills) -> {ps with rifle_kills = rifle_kills}
  | ("deaths", INT deaths) -> {ps with deaths = deaths}
  | ("suicides", INT suicides) -> {ps with suicides = suicides}
  | ("flag_grabs", INT flag_grabs) -> {ps with flag_grabs = flag_grabs}
  | ("flag_captures", INT flag_captures) -> {ps with flag_captures = flag_captures}
  | ("flag_returns", INT flag_returns) -> {ps with flag_returns = flag_returns}
  | ("flag_carrier_kills", INT flag_carrier_kills) -> {ps with flag_carrier_kills = flag_carrier_kills}
  | _ -> ps in
  db_entity_of_row empty named_row [fill_player_stats; additional_cols]

let of_row named_row =
  of_row_e named_row (fun s c -> s)

let select_stmt =
  "select count(*) as total_games, sum(hammer_kills) as hammer_kills, sum(gun_kills) as gun_kills, sum(shotgun_kills) as shotgun_kills, sum(grenade_kills) as grenade_kills, sum(rifle_kills) as rifle_kills, sum(deaths) as deaths, sum(suicides) as suicides, sum(flag_grabs) as flag_grabs, sum(flag_captures) as flag_captures, sum(flag_returns) as flag_returns, sum(flag_carrier_kills) as flag_carrier_kills " ^
  "from game_players inner join players on players.id = game_players.player_id " ^
  "where players.name = ?"

let select player_name: (t * int64) option =
  let total_games = ref Int64.minus_one in
  let fill_total_games gp c = match c with
  | ("total_games", Sqlite3.Data.INT tg) -> total_games := tg; gp
  | _ -> gp in
  let prepared_stmt = prepare_bind_stmt select_stmt [Sqlite3.Data.TEXT player_name] in
  let player_stats_with_total_games r =
    let gp = of_row_e r fill_total_games in
    (gp, !total_games) in
  Option.map player_stats_with_total_games (exec_select_single_row_stmt prepared_stmt)
