let port: int ref = ref 12488
let address: string ref = ref "127.0.0.1"
let database_file: string ref = ref "./teeworlds_ratings.db"

let usage = "Usage: " ^ Sys.argv.(0) ^ " [-a <address>] [-p <port>] [-d <database>]"

let cl_arguments = [
  ("-p", Arg.Set_int(port), "Source port for the server");
  ("-d", Arg.Set_string(database_file), "Location of the sqlite3 database");
  ("-a", Arg.Set_string(address), "Address for server to bind on");
]

let _ =
  let _ = Arg.parse cl_arguments (fun x -> ()) usage in
  if !port <= 0 then
    raise (Failure ("Port cannot be negative: " ^ (string_of_int !port)))
  else if not (Sys.file_exists !database_file) then
    raise (Failure ("Database file does not exist: " ^ !database_file))
  else
    Server.run (!address, !port) !database_file
