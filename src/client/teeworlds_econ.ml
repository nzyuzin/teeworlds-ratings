open Nettelnet_client

type state = Start | Password_sent | Command_sent

let execute_command addr password command =
  let session = new telnet_session in
  let state = ref Start in

  let got_input is_urgent =
    let oq = session # output_queue in
    let iq = session # input_queue in
    let process_data str = match !state with
      | Start ->
        if Str.string_match (Str.regexp_string "Enter password:") str 0 then begin
          Queue.add (Telnet_data(password ^ "\n")) oq;
          session # update();
          state := Password_sent
        end
      | Password_sent ->
          Queue.add (Telnet_data(command ^ "\n")) oq;
          session # update();
          state := Command_sent
      | Command_sent -> Queue.add Telnet_eof oq in
    while Queue.length iq > 0 do
      let cmd = Queue.take iq in
      match cmd with
      | Telnet_will _
      | Telnet_wont _
      | Telnet_do _
      | Telnet_dont _ ->
          session # process_option_command cmd
      | Telnet_data s -> process_data s
      | Telnet_eof -> ()
      | _ -> ()
    done
  in

  let opts = session # get_options in
  session # set_options { opts with verbose_input = false; verbose_output = false };
  session # set_connection (Telnet_connect addr);
  session # set_callback got_input;
  session # attach ();
  session # run ()
