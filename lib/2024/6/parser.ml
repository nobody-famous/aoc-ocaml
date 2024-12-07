let rec line_to_grid row col grid line =
  match line with
  | ch :: rest ->
      Hashtbl.replace grid (row, col) ch;
      line_to_grid row (col + 1) grid rest
  | [] -> ()

let rec lines_to_grid row grid lines =
  match lines with
  | line :: rest ->
      line_to_grid row 0 grid (line |> String.to_seq |> List.of_seq);
      lines_to_grid (row + 1) grid rest
  | [] -> grid

let parse lines = lines |> lines_to_grid 0 (Hashtbl.create 16)
