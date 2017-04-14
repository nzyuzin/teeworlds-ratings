let read_file (filename: string): string list =
  if Sys.file_exists filename then
    let file = open_in filename in
    let result = Std.input_list file in
    let _ = close_in file in
    List.filter (fun s -> not (ExtString.String.starts_with s "#")) result
  else
    raise (Failure "No file named destinations.txt found in the current directory")

let get_destinations (filename: string): (string * int) list =
  let parse_destination (destination: string): (string * int) =
    let colon_pos = String.index destination ':' in
    let ip = String.sub destination 0 colon_pos in
    let port = String.sub destination (colon_pos + 1) (String.length destination - colon_pos - 1) in
    (ip, int_of_string(port)) in
  let destination_strings = read_file filename in
  List.map parse_destination destination_strings
