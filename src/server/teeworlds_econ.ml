exception UnexpectedInput of string

let execute_command (conn: Network.connection) (cmd: string)  =
  Network.output_string conn (cmd ^ "\n")

let connect (addr: Network.address) (password: string) (mutex: Mutex.t): Network.connection =
  let conn = Network.create_connection addr in
  let password_line = Network.input_line conn in
  let _ = if password_line != "Enter password:" then
    raise (UnexpectedInput password_line)
  else
    execute_command conn (password ^ "\n") in
  conn
