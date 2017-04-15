let json_pretty_to_string json = Yojson.Basic.pretty_to_string json
let json_of_string str = Yojson.Basic.from_string str

let rec json_of_players (players: Gameinfo.tplayer list) =
  let open Gameinfo in
  match players with
  | [] -> []
  | player :: rest -> let open Yojson.Basic in
    `Assoc([
      ("name", `String(player.name));
      ("clan", `String(player.clan));
      ("score", `Int(player.score));
      ("team", `String(string_of_team(player.team)))
    ]) :: (json_of_players rest)

let json_of_gameinfo (gameinfo: Gameinfo.tgameinfo) =
  let open Gameinfo in
  `Assoc([
    ("gametype", `String(gameinfo.gametype));
    ("winner", `String(string_of_team(gameinfo.winner)));
    ("players", `List(json_of_players gameinfo.players))
  ])

let rec players_of_json (players: Yojson.Basic.json list): Gameinfo.tplayer list =
  let open Gameinfo in
  match players with
  | [] -> []
  | `Assoc([
      ("name", `String(nm));
      ("clan", `String(cn));
      ("score", `Int(scr));
      ("team", `String(tm))
    ]) :: rest ->
      let player = {name = nm; clan = cn; score = scr; team = team_of_string tm} in
      player :: (players_of_json rest)
  | json :: rest ->
      raise (Failure ("Unexpected input for players:\n" ^ (json_pretty_to_string json)))

let gameinfo_of_json (gameinfo: Yojson.Basic.json): Gameinfo.tgameinfo =
  let open Gameinfo in
  match gameinfo with
  | `Assoc([
      ("gametype", `String(gt));
      ("winner", `String(wnr));
      ("players", `List(plrs))
    ]) -> {gametype = gt; winner = team_of_string wnr; players = players_of_json plrs}
  | json -> raise (Failure ("Unexpected input for gameinfo:\n" ^ (json_pretty_to_string json)))

let json_to_channel chan json = Yojson.Basic.to_channel chan json
