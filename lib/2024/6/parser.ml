let rec line_to_grid row col grid line =
  match line with
  | ch :: rest -> grid |> Utils.Grid.add (row, col) ch |> fun g -> line_to_grid row (col + 1) g rest
  | [] -> grid

let rec lines_to_grid row grid lines =
  match lines with
  | line :: rest ->
      let new_grid = line_to_grid row 0 grid (line |> String.to_seq |> List.of_seq) in
      lines_to_grid (row + 1) new_grid rest
  | [] -> grid

let parse lines = lines |> lines_to_grid 0 Utils.Grid.empty
