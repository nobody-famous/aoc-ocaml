open Utils

let steps moons step_count =
  let rec loop count stop ms =
    Printf.printf "%d %d,%d,%d\n" count (List.nth ms 2).pos.x
      (List.nth ms 2).pos.y (List.nth ms 2).pos.z;

    if count < stop then
      let ms' = List.map (fun m -> compute_vel m ms) ms in
      let ms' = List.map (fun m -> apply_vel m) ms' in

      loop (count + 1) stop ms'
    else ms
  in

  loop 0 step_count moons

let run file_name =
  let moons = Parser.parse_input file_name in

  let _ = steps moons 50 in
  ()
