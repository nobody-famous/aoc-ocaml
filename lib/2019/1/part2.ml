open Parser
open Utils

let rec total_fuel mass =
  let fuel = fuel_for_mass mass in

  match fuel with _ when fuel <= 0 -> 0 | f -> f + total_fuel f

let run (file_name : string) : int =
  parse_input file_name |> List.map total_fuel |> List.fold_left ( + ) 0
