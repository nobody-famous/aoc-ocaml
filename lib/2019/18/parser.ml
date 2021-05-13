open AocUtils
open Utils

let parse_line ht row line =
  let rec loop col =
    if col < String.length line then (
      Hashtbl.replace ht { x = col; y = row } @@ char_to_piece line.[col];
      loop (col + 1))
  in

  loop 0

let build_map lines =
  let ht = Hashtbl.create 64 in
  let rec loop input row =
    match input with
    | [] -> ()
    | first :: rest ->
        parse_line ht row first;
        loop rest (row + 1)
  in

  loop lines 0;
  ht

let parse_input file_name = InputParser.read_lines file_name |> build_map
