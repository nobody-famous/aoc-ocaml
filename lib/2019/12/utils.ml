type coords = { x : int; y : int; z : int }

type cycle_state = {
  first : int;
  seen : int array;
  ndx : int;
  size : int option;
}

type moon = { pos : coords; vel : coords; cycles : cycle_state array }

let coords_to_string c = Printf.sprintf "{%d,%d,%d}" c.x c.y c.z

let moon_to_string m =
  Printf.sprintf "{pos=%s,vel=%s}" (coords_to_string m.pos)
    (coords_to_string m.vel)

let cycle_array item =
  let arr = Array.make 300000 0 in
  arr.(0) <- item;
  arr

let new_cycle_state first =
  let state = { first; seen = cycle_array first; ndx = 1; size = None } in
  state

let new_moon pos vel =
  let x_state = new_cycle_state pos.x in
  let y_state = new_cycle_state pos.y in
  let z_state = new_cycle_state pos.z in
  let cycles = [| x_state; y_state; z_state |] in

  { pos; vel; cycles }

let cycle_to_string cycle =
  let rec loop s ndx =
    if ndx < cycle.ndx then
      loop (Printf.sprintf "%s%d," s cycle.seen.(ndx)) @@ (ndx + 1)
    else s
  in

  let s = Printf.sprintf "{seen [" in
  let s = loop s 0 in
  let s = Printf.sprintf "%s]" s in
  let s =
    Printf.sprintf "%s,size %d}" s
    @@ match cycle.size with None -> 0 | Some s -> s
  in

  s

let has_cycle cycle = match cycle.size with Some _ -> true | None -> false

let check_for_loop cycle =
  let rec loop n n' =
    if n > n' then cycle.ndx > 1
    else if cycle.seen.(n) <> cycle.seen.(n') then false
    else loop (n + 1) (n' - 1)
  in

  let found = loop 0 (cycle.ndx - 1) in

  if found then { cycle with size = Some cycle.ndx } else cycle

let all_cycles moon =
  let has_all =
    Array.fold_left (fun acc c -> acc && has_cycle c) true moon.cycles
  in
  has_all

let add_to_cycle cycle item =
  match cycle.size with
  | Some _ -> cycle
  | None ->
      if cycle.ndx >= Array.length cycle.seen then
        failwith (Printf.sprintf "NDX %d\n" cycle.ndx);

      if has_cycle cycle then cycle
      else (
        cycle.seen.(cycle.ndx) <- item;
        { cycle with ndx = cycle.ndx + 1 })

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

  if (not @@ has_cycle m.cycles.(0)) && new_pos.x = m.cycles.(0).first then
    m.cycles.(0) <- check_for_loop m.cycles.(0);

  if (not @@ has_cycle m.cycles.(1)) && new_pos.y = m.cycles.(1).first then
    m.cycles.(1) <- check_for_loop m.cycles.(1);

  if (not @@ has_cycle m.cycles.(2)) && new_pos.z = m.cycles.(2).first then
    m.cycles.(2) <- check_for_loop m.cycles.(2);

  { m with pos = new_pos }
