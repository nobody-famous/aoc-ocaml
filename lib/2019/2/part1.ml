open Utils

let run file_name =
  let input = Intcode.parse_input file_name in
  let m = Intcode.new_machine () input in
  let m = Intcode.set_addr m 1 12 in
  let m = Intcode.set_addr m 2 2 in

  Intcode.get_addr (run_prog m) 0
