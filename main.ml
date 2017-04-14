type connection = in_channel * out_channel

let in_connection ((i, o): connection) = i
let out_connection ((i, o): connection) = o

let create_connection (ip: string) (port: int): connection =
  let addr = Unix.ADDR_INET (Unix.inet_addr_of_string ip, port) in
  Unix.open_connection addr

let close_connection (conn: connection): unit =
  let in_conn = in_connection conn in
  let _ = Unix.shutdown_connection in_conn in
  close_in in_conn

let debug: bool ref = ref false
let prdebug msg = if !debug then prerr_endline msg

let usage = "Usage: " ^ Sys.argv.(0) ^ " scores"

let scores: string ref = ref ""
let server_ip: string ref = ref ""
let server_port: int ref = ref (-1)

let cl_arguments = [
  ("-d", Arg.Set(debug), "Enables debug logging");
  ("-s", Arg.Set_string(server_ip), "IP to which the scores will be sent");
  ("-p", Arg.Set_int(server_port), "Port to which the scores will be sent");
]

let _ =
  let line_stream_of_string str =
    Stream.of_list (Str.split (Str.regexp "\n") str) in
  let _ = Arg.parse cl_arguments (fun scores' -> scores := scores') usage in
  let _ = prdebug ("Input:\n" ^ !scores ^ "\n") in
  let parsed_scores = Gameinfo.parse_gameinfo (line_stream_of_string !scores) in
  let jsoned_scores = Json.json_of_gameinfo parsed_scores in
  let _ = prdebug ("Output:\n" ^ (Json.json_pretty_to_string jsoned_scores) ^ "\n") in
  let _ = prdebug ("Sending message to " ^ !server_ip ^ ":" ^ (string_of_int !server_port)) in
  let conn = create_connection !server_ip !server_port in
  let out_conn = out_connection conn in
  let _ = Json.json_to_channel out_conn jsoned_scores in
  let _ = flush out_conn in
  close_connection conn

