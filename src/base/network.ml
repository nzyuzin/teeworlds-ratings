type address = string * int

let string_of_address addr = (fst addr) ^ ":" ^ (string_of_int (snd addr))

type connection = in_channel * out_channel

let in_connection ((i, o): connection) = i
let out_connection ((i, o): connection) = o

let create_connection (ip, port: address): connection =
  let addr = Unix.ADDR_INET (Unix.inet_addr_of_string ip, port) in
  Unix.open_connection addr

let close_connection (conn: connection): unit =
  let in_conn = in_connection conn in
  let _ = Unix.shutdown_connection in_conn in
  close_in in_conn

let establish_server handler addr =
  Unix.establish_server (fun in_conn out_conn ->  handler (in_conn, out_conn)) addr

let transfer_json addr json =
  let conn = create_connection addr in
  let out_conn = out_connection conn in
  let _ = Json.json_to_channel out_conn json in
  let _ = flush out_conn in
  close_connection conn
