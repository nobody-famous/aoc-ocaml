let rec walk_cols row col (line : char list) grid =
  match line with
  | ch :: rest ->
      grid.(row).(col) <- ch;
      walk_cols row (col + 1) rest grid
  | _ -> grid

let rec walk_rows row lines grid =
  match lines with
  | line :: rest -> walk_rows (row + 1) rest @@ walk_cols row 0 (line |> String.to_seq |> List.of_seq) grid
  | _ -> grid

let populate_grid (grid, lines) = walk_rows 0 lines grid
let create_grid lines = (Array.make_matrix (List.length lines) (String.length (List.hd lines)) '.', lines)
let parse_input lines = lines |> create_grid |> populate_grid
