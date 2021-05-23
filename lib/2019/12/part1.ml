open Utils

let energy c = Int.abs c.x + Int.abs c.y + Int.abs c.z

let total_energy m = energy m.pos * energy m.vel

let steps step_count moons =
  let rec loop count stop ms =
    if count < stop then
      ms
      |> List.map (fun m -> compute_vel m ms)
      |> List.map (fun m -> apply_vel m)
      |> loop (count + 1) stop
    else ms
  in

  loop 0 step_count moons

let run file_name =
  Parser.parse_input file_name
  |> steps 1000
  |> List.fold_left (fun acc m -> acc + total_energy m) 0
