open Parser
open Utils

let run (file_name : string) : int =
  let input = parse_input file_name in

  List.fold_left ( + ) 0 (fuel_required input)
