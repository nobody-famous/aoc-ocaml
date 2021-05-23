open Parser
open Utils

let run (file_name : string) : int =
  parse_input file_name |> fuel_required |> List.fold_left ( + ) 0
