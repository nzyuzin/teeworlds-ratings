let usage = "Usage: " ^ Sys.executable_name ^ " message"

let teeworlds_message: string ref = ref ""

let _ =
  let open Config_file in
  let config_options = new group in
  let destination = new string_cp ~group:config_options ["ratings_server_address"]
    ~short_name:"a"
    "127.0.0.1:12488"
    "Address of a teeworlds-ratings server to which the messages should be sent format: addr:port" in
  let econ_port = new int_cp ~group:config_options ["econ_port"] ~short_name:"e"
    18383
    "Econ port of the teeworlds server which will be used to send callbacks to" in
  let econ_password = new string_cp ~group:config_options ["econ_password"]
    ~short_name:"p"
    "password"
    "Econ password of the teeworlds server" in
  let debug = new bool_cp ~group:config_options ["debug"] ~short_name:"d" false "Enables debug logging" in
  let _ = config_options # read (Sys.executable_name ^ ".conf") in
  let cl_arguments = config_options # command_line_args "-" in
  let _ = Arg.parse cl_arguments (fun tw_message -> teeworlds_message := tw_message) usage in
  let addr = Network.address_of_string (destination # get) in
  Client.run !teeworlds_message addr (econ_port # get) (econ_password # get) (debug # get)
