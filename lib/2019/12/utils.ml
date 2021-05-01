type coords = { x : int; y : int; z : int }

type moon = { pos : coords; vel : coords }

let coords_to_string c = Printf.sprintf "{%d,%d,%d}" c.x c.y c.z

let moon_to_string m =
  Printf.sprintf "{pos=%s,vel=%s}" (coords_to_string m.pos)
    (coords_to_string m.vel)

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
