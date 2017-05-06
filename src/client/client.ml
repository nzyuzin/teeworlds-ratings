exception ServerError of string
exception UnrecognizedServerReponse of string

let is_debug = Global.empty "client_debug"

let prdebug msg = if (Global.get is_debug) then prerr_endline msg

let send addr json =
  let _ = prdebug ("Sending message to " ^ (Network.string_of_address addr)) in
  let conn = Network.create_connection addr in
  let out_conn = Network.out_connection conn in
  let _ = Json.to_channel out_conn json in
  let _ = output_char out_conn '\n' in
  let _ = flush out_conn in
  let server_response_line = input_line (Network.in_connection conn) in
  let server_response = Json.from_string server_response_line in
  let _ = prdebug ("Response:\n" ^ (Json.json_pretty_to_string server_response) ^ "\n") in
  let response_teeworlds_message = match Json.to_message server_response with
    | Json.Message("teeworlds_message", body) -> body
    | Json.Message(something_else, _) -> raise (UnrecognizedServerReponse something_else)
    | Json.Error msg -> raise (ServerError msg) in
  let result = Json.server_response_of_json response_teeworlds_message in
  let _ = Network.close_connection conn in
  result

let send_message message addr =
  let _ = prdebug ("Request:\n" ^ (Json.json_pretty_to_string message) ^ "\n") in
  send addr message

let communicate_with_server json addr econ_port econ_password =
  match send_message json addr with
  | Teeworlds_message.Acknowledge -> ()
  | Teeworlds_message.Callback str ->
      Teeworlds_econ.execute_command ("127.0.0.1", econ_port) econ_password str

let run teeworlds_message addr econ_port econ_password debug =
  let report_error str = Teeworlds_econ.execute_command
    ("127.0.0.1", econ_port) econ_password ("_cb_error \"" ^ (String.escaped str) ^ "\"") in
  let _ = Global.set is_debug debug in
  let _ = prdebug ("Input:\n" ^ teeworlds_message ^ "\n") in
  let parsed_message = Teeworlds_message.parse_message teeworlds_message in
  let teeworlds_message_json = Json.json_of_teeworlds_message parsed_message in
  let json = Json.of_message (Json.Message ("teeworlds_message", teeworlds_message_json)) in
  try
    communicate_with_server json addr econ_port econ_password
  with
  | ServerError str -> report_error str
