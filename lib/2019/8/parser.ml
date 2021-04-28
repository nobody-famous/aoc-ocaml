let parse_input file_name width height =
  let lines = InputParser.read_lines file_name in
  let matrix = Array.make_matrix width height 0 in
  let line = match lines with first :: _ -> first | [] -> "" in

  Printf.printf "line %s\n" line;

  let rec loop row =
    let rec col_loop col =
      if col < width then (
        let ndx = (row * width) + col in
        Printf.printf "%d,%d %c\n" row col (String.get line ndx);
        col_loop (col + 1))
    in

    if row < height then (
      col_loop 0;
      loop (row + 1))
  in

  loop 0;
  matrix
