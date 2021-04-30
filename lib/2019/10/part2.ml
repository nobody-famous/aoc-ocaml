open Utils

let find_station_map input =
  let _, map, _ =
    List.fold_left
      (fun (p, ht, best) p' ->
        let slope_map = build_map p' input in
        let count = count_asteroids slope_map in
        if count > best then (p', slope_map, count) else (p, ht, best))
      ((0, 0), Hashtbl.create 0, 0)
      input
  in

  map

let run file_name =
  let input = Parser.parse_input file_name in
  let map = find_station_map input in

  Hashtbl.iter
    (fun slope sides ->
      Printf.printf "%f pos: %d, neg: %d\n" slope (List.length sides.pos)
        (List.length sides.neg))
    map;
  0
