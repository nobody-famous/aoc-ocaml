open Utils

let parse_coords line =
  let str =
    Str.regexp "<x=\\(-?[0-9]+\\), y=\\(-?[0-9]+\\), z=\\(-?[0-9]+\\)>"
  in

  if Str.string_match str line 0 then
    let x = Str.matched_group 1 line in
    let y = Str.matched_group 2 line in
    let z = Str.matched_group 3 line in

    {
      pos = { x = int_of_string x; y = int_of_string y; z = int_of_string z };
      vel = { x = 0; y = 0; z = 0 };
    }
  else raise (Failure "INVALID INPUT")

let parse_input file_name =
  let lines = InputParser.read_lines file_name in

  List.map (fun line -> parse_coords line) lines
