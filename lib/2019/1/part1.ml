open Parser

let run (file_name : string) : int64 =
  let input = parse_input file_name in
  let fuel mass = Int64.sub (Int64.div mass 3L) 2L in
  let fuel_required = List.map fuel input in

  List.fold_left Int64.add 0L fuel_required
