open Parser

let run (file_name : string) : int =
  let input = parse_input file_name in
  let fuel mass = (mass / 3) - 2 in
  let fuel_required = List.map fuel input in

  List.fold_left ( + ) 0 fuel_required
