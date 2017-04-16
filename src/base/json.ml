let json_pretty_to_string json = Yojson.Basic.pretty_to_string json
let json_of_string str = Yojson.Basic.from_string str

let rec json_of_players (players: Gameinfo.player list) =
  match players with
  | [] -> []
  | player :: rest -> let open Yojson.Basic in
    `Assoc([
      ("name", `String(player.Gameinfo.name));
      ("clan", `String(player.Gameinfo.clan));
      ("score", `Int(player.Gameinfo.score));
      ("team", `String(Gameinfo.string_of_team(player.Gameinfo.team)))
    ]) :: (json_of_players rest)

let json_of_gameinfo (gameinfo: Gameinfo.gameinfo) =
  `Assoc([
    ("gametype", `String(gameinfo.Gameinfo.gametype));
    ("winner", `String(Gameinfo.string_of_team(gameinfo.Gameinfo.winner)));
    ("players", `List(json_of_players gameinfo.Gameinfo.players))
  ])

let rec players_of_json (players: Yojson.Basic.json list): Gameinfo.player list =
  match players with
  | [] -> []
  | `Assoc([
      ("name", `String(nm));
      ("clan", `String(cn));
      ("score", `Int(scr));
      ("team", `String(tm))
    ]) :: rest ->
      let player = {
        Gameinfo.name = nm;
        Gameinfo.clan = cn;
        Gameinfo.score = scr;
        Gameinfo.team = Gameinfo.team_of_string tm
      } in
      player :: (players_of_json rest)
  | json :: rest ->
      raise (Failure ("Unexpected input for players:\n" ^ (json_pretty_to_string json)))

let gameinfo_of_json (gameinfo: Yojson.Basic.json): Gameinfo.gameinfo =
  match gameinfo with
  | `Assoc([
      ("gametype", `String(gt));
      ("winner", `String(wnr));
      ("players", `List(plrs))
    ]) -> {
        Gameinfo.gametype = gt;
        Gameinfo.winner = Gameinfo.team_of_string wnr;
        Gameinfo.players = players_of_json plrs
      }
  | json -> raise (Failure ("Unexpected input for gameinfo:\n" ^ (json_pretty_to_string json)))

let json_to_channel chan json = Yojson.Basic.to_channel chan json
