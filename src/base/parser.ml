let until_char str chr from_pos =
  let end_pos = if String.contains_from str from_pos chr then
    String.index_from str from_pos chr
  else
    String.length str in
  String.sub str from_pos (end_pos - from_pos)

let until_space str from_pos =
  until_char str ' ' from_pos

let list_of_ints ints_str from_pos: int list =
  let str = String.trim ints_str in
  let len = String.length str in
  let rec inner prev =
    if prev >= len then
      []
    else
      let i = until_space str prev in
      (int_of_string i) :: (inner (prev + (String.length i) + 1)) in
  inner from_pos

let read_quoted_word str start_pos =
  let rec find_matching_quote pos =
    let first_quote = String.index_from str pos '"' in
    if str.[first_quote - 1] = '\\' then
      find_matching_quote (first_quote + 1)
    else
      first_quote in
  if str.[start_pos] != '"' then
    raise (Failure ("No quotation at '" ^ str ^ "'!"))
  else
    let end_quote = find_matching_quote (start_pos + 1) in
    String.sub str (start_pos + 1) (end_quote - start_pos - 1)

let unguard_quotes str =
  Str.global_replace (Str.regexp_string "\\\"") "\"" str
