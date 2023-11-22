open Utils

let run lines =
  let input = Parser.parse_input lines in

  List.fold_left
    (fun acc p ->
      let slope_map = build_map p input in
      Stdlib.max acc (count_visible slope_map))
    0 input
