open Parser

let rec do_mod x y = if x > 0 then x mod y else do_mod (x + y) y

let move width height robot =
  let new_x = do_mod (fst robot.position + fst robot.velocity) width in
  let new_y = do_mod (snd robot.position + snd robot.velocity) height in
  { robot with position = (new_x, new_y) }

let rec do_moves width height count robots =
  if count > 0 then do_moves width height (count - 1) (List.map (move width height) robots) else robots

let run_sized width height lines =
  let robots = lines |> parse_input |> do_moves width height 100 in
  List.iter
    (fun robot ->
      Printf.printf "%d,%d %d,%d\n" (fst robot.position) (snd robot.position) (fst robot.velocity) (snd robot.velocity))
    robots;
  Aoc.Utils.IntResult 0

let run lines = run_sized 101 103 lines
