let debug: bool ref = ref false
let prdebug msg = if !debug then prerr_endline msg

let usage = "Usage: " ^ Sys.argv.(0) ^ " scores"

let teeworlds_message: string ref = ref ""
let server_ip: string ref = ref ""
let server_port: int ref = ref (-1)
let use_threads: bool ref = ref false

let cl_arguments = [
  ("-d", Arg.Set(debug), "Enables debug logging");
  ("-s", Arg.Set_string(server_ip), "IP to which the scores will be sent");
  ("-p", Arg.Set_int(server_port), "Port to which the scores will be sent");
  ("-t", Arg.Set(use_threads), "Transfer messages to more than one endpoint in separate threads");
]

let _ =
  let send_msg addr msg =
    let _ = prdebug ("Sending message to " ^ (Network.string_of_address addr)) in
    try
      Network.transfer_json addr msg
    with exc ->
      prerr_endline ("Failure during connection to " ^ (Network.string_of_address addr)) in
  let _ = Arg.parse cl_arguments (fun tw_message -> teeworlds_message := tw_message) usage in
  let _ = prdebug ("Input:\n" ^ !teeworlds_message ^ "\n") in
  let parsed_message = Teeworlds_message.parse_message !teeworlds_message in
  let parsed_gameinfo = match parsed_message with
  | Teeworlds_message.Gameinfo gameinfo -> gameinfo
  | _ -> raise (Failure "Unsupported message") in
  let jsoned_gameinfo = Json.json_of_gameinfo parsed_gameinfo in
  let _ = prdebug ("Output:\n" ^ (Json.json_pretty_to_string jsoned_gameinfo) ^ "\n") in
  if !server_ip != "" && !server_port != -1 then
    send_msg (!server_ip, !server_port) jsoned_gameinfo
  else
    let destinations = Destinations.get_destinations "destinations.txt" in
    let transfer_gameinfo_new_thread addr =
      Thread.create (fun msg -> send_msg addr msg) jsoned_gameinfo in
    let transfer_gameinfo addr = send_msg addr jsoned_gameinfo in
    if List.length destinations > 1 then
      if !use_threads then
        let threads = List.map transfer_gameinfo_new_thread destinations in
        List.iter (fun t -> Thread.join t) threads
      else
        List.iter transfer_gameinfo destinations
    else
      transfer_gameinfo (List.hd destinations)
