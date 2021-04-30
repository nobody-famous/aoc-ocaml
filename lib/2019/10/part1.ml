open Utils

let run file_name =
  let input = Parser.parse_input file_name in

  List.fold_left
    (fun acc p ->
      let slope_map = build_map p input in
      Stdlib.max acc (count_visible slope_map))
    0 input
