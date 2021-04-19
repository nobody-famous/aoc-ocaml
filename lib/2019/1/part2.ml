open Parser
open Utils

let run (file_name : string) : int =
  let input = parse_input file_name in
  let rec total_fuel mass =
    let fuel = fuel_for_mass mass in

    match fuel with _ when fuel <= 0 -> 0 | f -> f + total_fuel f
  in

  let totals = List.map total_fuel input in
  List.fold_left ( + ) 0 totals
