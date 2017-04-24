let port: int ref = ref 12488
let database_file: string ref = ref "./rctf.db"
let econ_password: string ref = ref "test"

let usage = "Usage: " ^ Sys.argv.(0) ^ " [-p <port>] [-d <database>]"

let cl_arguments = [
  ("-p", Arg.Set_int(port), "Source port for the server");
  ("-d", Arg.Set_string(database_file), "Location of the sqlite3 database");
  ("-e", Arg.Set_string(econ_password), "Teeworlds econ password");
]

let _ =
  let _ = Arg.parse cl_arguments (fun x -> ()) usage in
  if !port <= 0 then
    raise (Failure ("Port cannot be negative: " ^ (string_of_int !port)))
  else if not (Sys.file_exists !database_file) then
    raise (Failure ("Database file does not exist: " ^ !database_file))
  else
    Server.run !port !database_file !econ_password
