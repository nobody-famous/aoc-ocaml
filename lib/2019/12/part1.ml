open Utils

let energy c = Int.abs c.x + Int.abs c.y + Int.abs c.z

let total_energy m = energy m.pos * energy m.vel

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
  let moons = Parser.parse_input file_name in
  let moons = steps moons 1000 in

  List.fold_left (fun acc m -> acc + total_energy m) 0 moons
