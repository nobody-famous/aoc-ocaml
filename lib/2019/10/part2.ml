open Utils

type quadrant = NORTH_EAST | SOUTH_EAST | NORTH_WEST | SOUTH_WEST

type slope_point = { slope : float; quad : quadrant; points : point array }

let quad_to_string quad =
  match quad with
  | NORTH_EAST -> "NORTH_EAST"
  | SOUTH_EAST -> "SOUTH_EAST"
  | NORTH_WEST -> "NORTH_WEST"
  | SOUTH_WEST -> "SOUTH_WEST"

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

let slope_to_quadrant slope point origin =
  if (slope < 0. || slope = Float.infinity) && point.y < origin.y then
    NORTH_EAST
  else if slope >= 0. && point.x > origin.x then SOUTH_EAST
  else if (slope < 0. || slope = Float.infinity) && point.y > origin.y then
    SOUTH_WEST
  else if slope >= 0. && point.x <= origin.x then NORTH_WEST
  else raise (Failure "Could not assign quadrant")

let sort_points points origin =
  Array.sort
    (fun p1 p2 ->
      let dist1 = man_dist p1 origin in
      let dist2 = man_dist p2 origin in

      compare dist1 dist2)
    points

let add_sides acc slope sides origin =
  let acc =
    if List.length sides.pos > 0 then (
      let points = Array.of_list sides.pos in
      let quad = slope_to_quadrant slope (List.nth sides.pos 0) origin in

      sort_points points origin;

      { slope; points; quad } :: acc)
    else acc
  in

  let acc =
    if List.length sides.neg > 0 then (
      let points = Array.of_list sides.neg in
      let quad = slope_to_quadrant slope (List.nth sides.neg 0) origin in

      sort_points points origin;
      { slope; points; quad } :: acc)
    else acc
  in

  acc

let compare_quadrant q1 q2 =
  if q1 = q2 then 0
  else
    match q1 with
    | NORTH_EAST -> if q2 = NORTH_EAST then 0 else -1
    | SOUTH_EAST -> ( match q2 with NORTH_EAST -> 1 | _ -> -1)
    | SOUTH_WEST -> ( match q2 with NORTH_WEST -> -1 | _ -> 1)
    | NORTH_WEST -> 1

let compare_slopes s1 s2 = compare s1.quad s2.quad

let map_to_array map origin =
  let items = Hashtbl.to_seq map in

  let point_data =
    Seq.fold_left
      (fun acc (slope, sides) -> add_sides acc slope sides origin)
      [] items
  in

  let point_array = Array.of_list point_data in
  Array.sort compare_slopes point_array;

  Printf.printf "origin %d %d\n" origin.x origin.y;
  Array.iter
    (fun p ->
      Printf.printf "%f %d %s\n" p.slope (Array.length p.points)
        (quad_to_string p.quad))
    point_array;

  Printf.printf "length %d\n" (List.length point_data);

  ()

let run file_name =
  let input = Parser.parse_input file_name in
  let origin, map = find_station_map input in
  let count = count_visible map in

  map_to_array map origin;

  Printf.printf "%d,%d %d\n" origin.x origin.y count;

  0
