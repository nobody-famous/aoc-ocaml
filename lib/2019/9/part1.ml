open Utils

let run file_name =
  Intcode.parse_input file_name
  |> Intcode.new_machine () |> Intcode.set_input 1 |> run_mach 0
