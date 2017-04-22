exception NoDestinationsFile

let read_file (filename: string): string list =
  if Sys.file_exists filename then
    let file = open_in filename in
    let result = Std.input_list file in
    let _ = close_in file in
    List.filter (fun s -> not (ExtString.String.starts_with s "#")) result
  else
    raise NoDestinationsFile

let get_destinations (filename: string): (string * int) list =
  let destination_strings = read_file filename in
  List.map Network.address_of_string destination_strings
