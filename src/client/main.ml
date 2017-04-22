let usage = "Usage: " ^ Sys.argv.(0) ^ " scores"

let debug: bool ref = ref false

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
  let _ = Arg.parse cl_arguments (fun tw_message -> teeworlds_message := tw_message) usage in
  Client.run !teeworlds_message !server_ip !server_port !use_threads !debug
