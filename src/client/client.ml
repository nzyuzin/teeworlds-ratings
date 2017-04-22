let is_debug = Global.empty "client_debug"

let prdebug msg = if (Global.get is_debug) then prerr_endline msg

let send addr msg =
  let _ = prdebug ("Sending message to " ^ (Network.string_of_address addr)) in
  try
    Network.transfer_json addr msg
  with exc ->
    prerr_endline ("Failure during connection to " ^ (Network.string_of_address addr))

let send_to_destinations message use_threads =
  let destinations = Destinations.get_destinations "destinations.txt" in
  let transfer_message_new_thread addr =
    Thread.create (fun msg -> send addr msg) message in
  let transfer_message addr = send addr message in
  if List.length destinations > 1 then
    if use_threads then
      let threads = List.map transfer_message_new_thread destinations in
      List.iter (fun t -> Thread.join t) threads
    else
      List.iter transfer_message destinations
  else
    transfer_message (List.hd destinations)

let send_message message server_ip server_port use_threads =
  let _ = prdebug ("Output:\n" ^ (Json.json_pretty_to_string message) ^ "\n") in
  if server_ip != "" && server_port != -1 then
    send (server_ip, server_port) message
  else
    send_to_destinations message use_threads

let process_gameinfo gameinfo server_ip server_port use_threads =
  let jsoned_gameinfo = Json.json_of_gameinfo gameinfo in
  send_message jsoned_gameinfo

let run teeworlds_message server_ip server_port use_threads debug =
  let _ = Global.set is_debug debug in
  let _ = prdebug ("Input:\n" ^ teeworlds_message ^ "\n") in
  let parsed_message = Teeworlds_message.parse_message teeworlds_message in
  match parsed_message with
  | Teeworlds_message.Gameinfo gameinfo ->
      process_gameinfo gameinfo server_ip server_port use_threads
  | _ -> raise (Failure "Unsupported message")
