open AocUtils
open Utils

let add_to_data pt piece data =
  match piece with
  | KEY k ->
      Hashtbl.replace data.keys pt k;
      data
  | DOOR d ->
      Hashtbl.replace data.doors pt d;
      data
  | EMPTY ->
      Hashtbl.replace data.empty pt '.';
      data
  | ENTRANCE -> { data with enter = Some pt }
  | _ -> data

let parse_line row line data =
  let rec loop col data' =
    if col >= String.length line then data'
    else
      let piece = char_to_piece line.[col] in
      let pt = { x = col; y = row } in

      loop (col + 1) @@ add_to_data pt piece data'
  in

  loop 0 data

let build_map lines =
  let rec loop input row data =
    match input with
    | [] -> data
    | first :: rest -> loop rest (row + 1) @@ parse_line row first data
  in

  loop lines 0 @@ new_pieces ()

let parse_input file_name = InputParser.read_lines file_name |> build_map
