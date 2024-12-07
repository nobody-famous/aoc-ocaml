open Utils

let run lines =
  Aoc.Utils.IntResult (Intcode.parse_input lines |> Intcode.new_machine () |> Intcode.set_input 2 |> run_mach 0)
