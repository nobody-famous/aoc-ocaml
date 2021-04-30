open Utils

let run file_name =
  let input = Parser.parse_input file_name in

  List.fold_left (fun acc p -> Stdlib.max acc (count_asteroids p input)) 0 input
