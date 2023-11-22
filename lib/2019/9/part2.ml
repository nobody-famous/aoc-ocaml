open Utils

let run lines =
  Intcode.parse_input lines
  |> Intcode.new_machine ()
  |> Intcode.set_input 2
  |> run_mach 0
