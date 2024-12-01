open Parser
open Utils

let run lines = Aoc.Utils.IntResult (parse_input lines |> fuel_required |> List.fold_left ( + ) 0)
