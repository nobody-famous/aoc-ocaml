let get_guard_path (start, grid) = (start, grid) |> Utils.walk_path |> fun path -> (start, grid, path)

let is_loop start grid =
  let path = Utils.walk_path (start, grid) in
  Utils.Points.cardinal path = 0

let count_loops (start, grid, path) =
  Utils.Points.fold
    (fun pt acc -> acc + if pt <> start && is_loop start (Utils.Grid.add pt '#' grid) then 1 else 0)
    path 0

let run lines = Aoc.Utils.IntResult (lines |> Parser.parse |> Utils.find_start |> get_guard_path |> count_loops)
