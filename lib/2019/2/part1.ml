open Utils

let run file_name =
  let input = Intcode.parse_input file_name in
  let m =
    Intcode.new_machine () input
    |> Intcode.set_addr 1 12 |> Intcode.set_addr 2 2
  in

  Intcode.get_addr (run_prog m) 0
