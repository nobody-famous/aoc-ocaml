type map_item = Empty | Asteroid

let char_to_item ch =
  match ch with
  | '.' -> Empty
  | '#' -> Asteroid
  | _ -> raise (Failure "Invalid Map Item")

let build_matrix lines =
  let matrix =
    Array.make_matrix (List.length lines)
      (String.length (List.nth lines 0))
      Empty
  in

  let rec loop rem_lines row =
    match rem_lines with
    | [] -> ()
    | first :: rest ->
        let rec col_loop col =
          if col < String.length first then (
            matrix.(row).(col) <- char_to_item (String.get first col);
            col_loop (col + 1))
        in

        col_loop 0;
        loop rest (row + 1)
  in

  loop lines 0;
  matrix

let parse_input file_name =
  let lines = InputParser.read_lines file_name in

  build_matrix lines
