let parse_input (file_name : string) : int64 list =
  let in_channel = open_in file_name in
  let try_read () =
    try Some (input_line in_channel) with End_of_file -> None
  in
  let rec loop acc =
    match try_read () with
    | Some s -> loop (s :: acc)
    | None ->
        close_in in_channel;
        List.rev acc
  in

  List.map Int64.of_string (loop [])
