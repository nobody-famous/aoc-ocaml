open Utils

type quadrant = NORTH_EAST | SOUTH_EAST | NORTH_WEST | SOUTH_WEST
type slope_point = { slope : float; quad : quadrant; points : point list }

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
  else failwith "Could not assign quadrant"

let sort_points points origin =
  if List.length points = 1 then points
  else
    let arr = Array.of_list points in

    Array.sort
      (fun p1 p2 ->
        let dist1 = man_dist origin p1 in
        let dist2 = man_dist origin p2 in

        compare dist1 dist2)
      arr;

    Array.to_list arr

let add_sides acc slope sides origin =
  let acc =
    if List.length sides.pos > 0 then
      let quad = slope_to_quadrant slope (List.nth sides.pos 0) origin in
      let points = sort_points sides.pos origin in

      { slope; points; quad } :: acc
    else acc
  in

  let acc =
    if List.length sides.neg > 0 then
      let quad = slope_to_quadrant slope (List.nth sides.neg 0) origin in
      let points = sort_points sides.neg origin in

      { slope; points; quad } :: acc
    else acc
  in

  acc

let compare_quadrant q1 q2 =
  if q1 = q2 then 0
  else
    match q1 with
    | NORTH_EAST -> -1
    | SOUTH_EAST -> (
        match q2 with
        | NORTH_EAST -> 1
        | _ -> -1)
    | SOUTH_WEST -> (
        match q2 with
        | NORTH_WEST -> -1
        | _ -> 1)
    | NORTH_WEST -> 1

let compare_slopes s1 s2 =
  if s1.quad = s2.quad then
    if s1.slope = Float.infinity then -1
    else if s2.slope = Float.infinity then 1
    else if s1.slope = Float.infinity then -1
    else compare s1.slope s2.slope
  else compare_quadrant s1.quad s2.quad

let map_to_array (origin, map) =
  let point_array =
    Hashtbl.to_seq map
    |> Seq.fold_left
         (fun acc (slope, sides) -> add_sides acc slope sides origin)
         []
    |> Array.of_list
  in

  Array.sort compare_slopes point_array;

  point_array

let sum_asteroids arr =
  Array.fold_left (fun acc s -> acc + List.length s.points) 0 arr

let remove_asteroids arr =
  let total = sum_asteroids arr in

  let rec loop ndx count last =
    if count = total then None
    else if count = 200 then last
    else if ndx >= Array.length arr then loop 0 count last
    else
      let item = arr.(ndx) in
      match item.points with
      | [] -> loop (ndx + 1) count last
      | first :: rest ->
          arr.(ndx) <- { (arr.(ndx)) with points = rest };
          loop (ndx + 1) (count + 1) @@ Some first
  in

  loop 0 0 None

let run lines =
  let arr = Parser.parse_input lines |> find_station_map |> map_to_array in

  match remove_asteroids arr with
  | None -> 0
  | Some p -> (p.x * 100) + p.y
