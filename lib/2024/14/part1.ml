open Parser

let run_sized width height lines =
  Aoc.Utils.IntResult
    (lines |> parse_input |> Utils.do_moves width height 100 |> Utils.get_bounds width height |> Utils.calculate_safety)

let run lines = run_sized 101 103 lines
