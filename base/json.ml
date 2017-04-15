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
  let open Yojson.Basic in
  let open Gameinfo in
  `Assoc([
    ("gametype", `String(gameinfo.gametype));
    ("winner", `String(Gameinfo.string_of_team(gameinfo.winner)));
    ("players", `List(json_of_players gameinfo.players))
  ])

let json_to_channel chan json = Yojson.Basic.to_channel chan json

let json_pretty_to_string json = Yojson.Basic.pretty_to_string json
