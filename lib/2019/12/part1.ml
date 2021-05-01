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

let run file_name =
  let moons = Parser.parse_input file_name in

  let moons = List.map (fun m -> compute_vel m moons) moons in

  Printf.printf "AFTER VEL\n";
  List.iter (fun m -> Printf.printf "%s\n" (moon_to_string m)) moons;

  let moons = List.map (fun m -> apply_vel m) moons in

  Printf.printf "AFTER GRAVITY\n";
  List.iter (fun m -> Printf.printf "%s\n" (moon_to_string m)) moons;

  let moons = List.map (fun m -> compute_vel m moons) moons in

  Printf.printf "VEL\n";
  List.iter (fun m -> Printf.printf "%s\n" (moon_to_string m)) moons;

  let moons = List.map (fun m -> apply_vel m) moons in

  Printf.printf "GRAVITY\n";
  List.iter (fun m -> Printf.printf "%s\n" (moon_to_string m)) moons
