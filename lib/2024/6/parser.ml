let rec line_to_grid row col grid line =
  match line with
  | ch :: rest ->
      grid.(row).(col) <- ch;
      line_to_grid row (col + 1) grid rest
  | [] -> ()

let rec lines_to_grid row (lines, grid) =
  match lines with
  | line :: rest ->
      line_to_grid row 0 grid (line |> String.to_seq |> List.of_seq);
      lines_to_grid (row + 1) (rest, grid)
  | [] -> grid

let make_tuple x y = (x, y)
let get_cols lines = lines |> List.hd |> String.length |> make_tuple lines
let get_rows (lines, cols) = make_tuple lines (List.length lines, cols)
let to_grid (lines, (rows, cols)) = make_tuple lines @@ Array.make_matrix rows cols '.'
let create_grid lines = lines |> get_cols |> get_rows |> to_grid
let parse lines = lines |> create_grid |> lines_to_grid 0
