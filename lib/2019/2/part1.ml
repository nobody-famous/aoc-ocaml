open Utils

let run lines =
  Intcode.parse_input lines
  |> Intcode.new_machine ()
  |> Intcode.set_addr 1 12
  |> Intcode.set_addr 2 2
  |> run_prog
  |> Intcode.get_addr 0
