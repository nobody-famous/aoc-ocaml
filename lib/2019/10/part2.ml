open Utils

type slope_list = { slope : float; points : point array }

let find_station_map input =
  let point, map, _ =
    List.fold_left
      (fun (p, ht, best) p' ->
        let slope_map = build_map p' input in
        let count = count_visible slope_map in
        if count > best then (p', slope_map, count) else (p, ht, best))
      ({ x = 0; y = 0 }, Hashtbl.create 0, 0)
      input
  in

  (point, map)

let add_sides acc slope sides =
  let out =
    if List.length sides.pos > 0 then
      { slope; points = Array.of_list sides.pos } :: acc
    else acc
  in

  let out =
    if List.length sides.neg > 0 then
      { slope; points = Array.of_list sides.neg } :: out
    else out
  in

  out

type quadrant = NORTH_EAST | SOUTH_EAST | NORTH_WEST | SOUTH_WEST

let slope_to_quadrant s =
  if (s.slope > 0. || s.slope = Float.infinity) && s.points.(0).y > 0 then
    NORTH_EAST
  else if s.slope <= 0. && s.points.(0).y <= 0 then SOUTH_EAST
  else if (s.slope > 0. || s.slope = Float.infinity) && s.points.(0).y < 0 then
    SOUTH_WEST
  else if s.slope <= 0. && s.points.(0).y >= 0 then NORTH_WEST
  else raise (Failure "Could not assign quadrant")

let compare_quadrant q1 q2 =
  if q1 = q2 then 0
  else
    match q1 with
    | NORTH_EAST -> if q2 = NORTH_EAST then 0 else -1
    | SOUTH_EAST -> ( match q2 with NORTH_EAST -> 1 | _ -> -1)
    | SOUTH_WEST -> ( match q2 with NORTH_WEST -> -1 | _ -> 1)
    | NORTH_WEST -> 1

let map_to_array map =
  let items = Hashtbl.to_seq map in

  let slope_lists =
    Seq.fold_left (fun acc (slope, sides) -> add_sides acc slope sides) [] items
  in

  Printf.printf "length %d\n" (List.length slope_lists);
  ()

let run file_name =
  let input = Parser.parse_input file_name in
  let origin, map = find_station_map input in
  let count = count_visible map in

  map_to_array map;

  Printf.printf "%d,%d %d\n" origin.x origin.y count;

  0
