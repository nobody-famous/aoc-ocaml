open Parser

let count_quadrants (areas, robots) = List.map (fun area -> Utils.count_robots area robots) areas

let rec count_moves width height count smallest robots =
  if count >= 7200 then
    count
  else
    let new_robots = robots |> Utils.do_moves width height 1 in
    let safety = new_robots |> Utils.get_bounds width height |> Utils.calculate_safety in
    if safety < smallest then
      count
    else
      count_moves width height (count + 1) (Int.min smallest safety) new_robots

let run_sized width height lines = Aoc.Utils.IntResult (lines |> parse_input |> count_moves width height 1 Int.max_int)
let run lines = run_sized 101 103 lines
