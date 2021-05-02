type coords = { x : int; y : int; z : int }

type cycle_state = { seen : int list; size : int option }

type moon = { pos : coords; vel : coords; cycles : cycle_state array }

let coords_to_string c = Printf.sprintf "{%d,%d,%d}" c.x c.y c.z

let moon_to_string m =
  Printf.sprintf "{pos=%s,vel=%s}" (coords_to_string m.pos)
    (coords_to_string m.vel)

let new_moon pos vel =
  let x_state = { seen = [ pos.x ]; size = None } in
  let y_state = { seen = [ pos.y ]; size = None } in
  let z_state = { seen = [ pos.z ]; size = None } in
  let cycles = [| x_state; y_state; z_state |] in

  { pos; vel; cycles }

let cycle_to_string cycle =
  let rec loop s seen =
    match seen with
    | [] -> s
    | first :: rest -> loop (Printf.sprintf "%s%d," s first) rest
  in

  let s = Printf.sprintf "{seen [" in
  let s = loop s cycle.seen in
  let s = Printf.sprintf "%s]" s in
  let s =
    Printf.sprintf "%s,size %d}" s
      (match cycle.size with None -> 0 | Some s -> s)
  in

  s

let has_cycle cycle =
  match cycle.size with
  | Some _ -> true
  | None ->
      let arr = Array.of_list cycle.seen in
      let rec loop n n' =
        if n > n' then List.length cycle.seen > 1
        else if arr.(n) <> arr.(n') then false
        else loop (n + 1) (n' - 1)
      in

      loop 0 (Array.length arr - 1)

let all_cycles moon =
  let has_all =
    Array.fold_left (fun acc c -> acc && has_cycle c) true moon.cycles
  in

  has_all

let add_to_cycle cycle item =
  match cycle.size with
  | Some _ -> cycle
  | None ->
      let cycle' = { cycle with seen = item :: cycle.seen } in
      let cycle' =
        if has_cycle cycle' then
          { cycle' with size = Some (List.length cycle'.seen) }
        else cycle'
      in

      cycle'

let compute_vel m moons =
  let rec loop ms delta =
    match ms with
    | [] -> { m with vel = delta }
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

  m.cycles.(0) <- add_to_cycle m.cycles.(0) new_pos.x;
  m.cycles.(1) <- add_to_cycle m.cycles.(1) new_pos.y;
  m.cycles.(2) <- add_to_cycle m.cycles.(2) new_pos.z;

  { m with pos = new_pos }
