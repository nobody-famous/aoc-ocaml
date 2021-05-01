open Utils

type quadrant = NORTH_EAST | SOUTH_EAST | NORTH_WEST | SOUTH_WEST

type slope_point = { slope : float; quad : quadrant; point : point }

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
  Printf.printf "%d %d %f\n" point.x point.y slope;

  if (slope > 0. || slope = Float.infinity) && point.y < origin.y then
    NORTH_EAST
  else if slope <= 0. && point.y >= origin.y then SOUTH_EAST
  else if (slope > 0. || slope = Float.infinity) && point.y > origin.y then
    SOUTH_WEST
  else if slope <= 0. && point.y <= origin.y then NORTH_WEST
  else raise (Failure "Could not assign quadrant")

let add_sides acc slope sides origin =
  let pos =
    List.fold_left
      (fun acc p ->
        let quad = slope_to_quadrant slope (List.nth sides.pos 0) origin in
        { slope; quad; point = p } :: acc)
      [] sides.pos
  in
  let neg =
    List.fold_left
      (fun acc p ->
        let quad = slope_to_quadrant slope (List.nth sides.neg 0) origin in
        { slope; quad; point = p } :: acc)
      [] sides.neg
  in

  acc @ pos @ neg

let compare_quadrant q1 q2 =
  if q1 = q2 then 0
  else
    match q1 with
    | NORTH_EAST -> if q2 = NORTH_EAST then 0 else -1
    | SOUTH_EAST -> ( match q2 with NORTH_EAST -> 1 | _ -> -1)
    | SOUTH_WEST -> ( match q2 with NORTH_WEST -> -1 | _ -> 1)
    | NORTH_WEST -> 1

let map_to_array map origin =
  let items = Hashtbl.to_seq map in

  let point_data =
    Seq.fold_left
      (fun acc (slope, sides) -> add_sides acc slope sides origin)
      [] items
  in

  Printf.printf "origin %d %d\n" origin.x origin.y;
  List.iter
    (fun p ->
      Printf.printf "%d %d %s\n" p.point.x p.point.y (quad_to_string p.quad))
    point_data;

  Printf.printf "length %d\n" (List.length point_data);

  ()

let run file_name =
  let input = Parser.parse_input file_name in
  let origin, map = find_station_map input in
  let count = count_visible map in

  map_to_array map origin;

  Printf.printf "%d,%d %d\n" origin.x origin.y count;

  0
