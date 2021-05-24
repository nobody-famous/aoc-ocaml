open Utils

let parse_coords line =
  let str =
    Str.regexp "<x=\\(-?[0-9]+\\), y=\\(-?[0-9]+\\), z=\\(-?[0-9]+\\)>"
  in

  if Str.string_match str line 0 then
    let x = int_of_string @@ Str.matched_group 1 line in
    let y = int_of_string @@ Str.matched_group 2 line in
    let z = int_of_string @@ Str.matched_group 3 line in

    new_moon { x; y; z } { x = 0; y = 0; z = 0 }
  else failwith "INVALID NeedInput"

let parse_input file_name =
  InputParser.read_lines file_name |> List.map (fun line -> parse_coords line)
