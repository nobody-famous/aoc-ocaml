let read_lines file_name =
  let input = open_in file_name in
  let read_line () = try Some (input_line input) with End_of_file -> None in
  let rec read_all lines =
    match read_line () with
    | Some line -> read_all (line :: lines)
    | None ->
        close_in input;
        List.rev lines
  in

  read_all []
