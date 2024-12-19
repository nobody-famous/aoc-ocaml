open Parser

let rec count_moves width height count (answer, smallest) robots =
  if count >= 7200 then
    answer
  else
    let new_robots = robots |> Utils.do_moves width height 1 in
    let safety = new_robots |> Utils.get_bounds width height |> Utils.calculate_safety in
    if safety < smallest then
      count_moves width height (count + 1) (count, safety) new_robots
    else
      count_moves width height (count + 1) (answer, smallest) new_robots

let run_sized width height lines =
  Aoc.Utils.IntResult (lines |> parse_input |> count_moves width height 1 (0, Int.max_int))

let run lines = run_sized 101 103 lines
