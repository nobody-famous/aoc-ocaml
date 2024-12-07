open Parser
open Utils

let rec total_fuel mass =
  match fuel_for_mass mass with
  | f when f <= 0 -> 0
  | f -> f + total_fuel f

let run lines = Aoc.Utils.IntResult (parse_input lines |> List.map total_fuel |> List.fold_left ( + ) 0)
