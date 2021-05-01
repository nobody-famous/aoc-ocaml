open Utils

let compute_vel m moons =
  let rec loop ms delta =
    match ms with
    | [] -> { pos = m.pos; vel = delta }
    | first :: rest ->
        let dx = compare first.pos.x m.pos.x in
        let dy = compare first.pos.y m.pos.y in
        let dz = compare first.pos.z m.pos.z in

        loop rest { x = delta.x + dx; y = delta.y + dy; z = delta.z + dz }
  in

  loop moons m.vel

let apply_vel m =
  let new_pos =
    { x = m.pos.x + m.vel.x; y = m.pos.y + m.vel.y; z = m.pos.z + m.vel.z }
  in

  { m with pos = new_pos }

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
