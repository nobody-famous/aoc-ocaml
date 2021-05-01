open Utils

let steps moons step_count =
  let rec loop count stop ms =
    if count < stop then
      let ms' = List.map (fun m -> compute_vel m ms) ms in
      let ms' = List.map (fun m -> apply_vel m) ms' in

      loop (count + 1) stop ms'
    else ms
  in

  loop 0 step_count moons

let run file_name =
  let _ = Parser.parse_input file_name in

  Printf.printf "Part 2\n"
