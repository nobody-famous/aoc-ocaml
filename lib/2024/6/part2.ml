let get_guard_path (start, grid) = (start, grid) |> Utils.walk_path |> fun path -> (start, grid, path)

let is_loop start grid =
  let path = Utils.walk_path (start, grid) in
  Hashtbl.length path = 0

let count_loops (start, grid, path) =
  Hashtbl.fold
    (fun pt _ acc ->
      grid.(fst pt).(snd pt) <- '#';
      let inc = if pt <> start && Utils.is_loop start grid then 1 else 0 in
      grid.(fst pt).(snd pt) <- '.';
      acc + inc)
    path 0

let run lines = Aoc.Utils.IntResult (lines |> Parser.parse |> Utils.find_start |> get_guard_path |> count_loops)
