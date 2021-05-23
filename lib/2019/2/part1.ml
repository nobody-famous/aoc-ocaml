open Utils

let run file_name =
  Intcode.parse_input file_name
  |> Intcode.new_machine () |> Intcode.set_addr 1 12 |> Intcode.set_addr 2 2
  |> run_prog |> Intcode.get_addr 0
