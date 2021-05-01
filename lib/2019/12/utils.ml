type coords = { x : int; y : int; z : int }

type moon = { pos : coords; vel : coords }

let coords_to_string c = Printf.sprintf "{%d,%d,%d}" c.x c.y c.z

let moon_to_string m =
  Printf.sprintf "{pos=%s,vel=%s}" (coords_to_string m.pos)
    (coords_to_string m.vel)
