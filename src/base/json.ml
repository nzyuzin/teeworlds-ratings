exception UnknownValue of string
exception IllFormedJson of string

type t = Yojson.Basic.json

type message = Message of string * t | Error of string

let json_pretty_to_string json = Yojson.Basic.pretty_to_string json
let from_string str = Yojson.Basic.from_string str

let error_ill_formed expected got =
  IllFormedJson ("Unexpected json for " ^ expected ^ ":\n" ^ (json_pretty_to_string got))
let error_unknown_value field_name field_value =
  UnknownValue ("Unknown " ^ field_name ^ ": " ^ field_value)

let from_channel (chan: in_channel): t = Yojson.Basic.from_channel chan
let to_channel (chan: out_channel) (json: t): unit = Yojson.Basic.to_channel chan json

let to_message: t -> message = function
  | `Assoc([
      ("message_type", `String("error"));
      ("message_content", `String(errmsg));
    ]) -> Error errmsg
  | `Assoc([
      ("message_type", `String(message_type));
      ("message_content", rest);
    ]) -> Message (message_type, rest)
  | something_else -> raise (error_ill_formed "message" something_else)

let of_message : message -> t = function
  | Message (message_type, message_content) -> `Assoc([
      ("message_type", `String(message_type));
      ("message_content", message_content);
    ])
  | Error str -> `Assoc([
      ("message_type", `String("error"));
      ("message_content", `String(str));
    ])
