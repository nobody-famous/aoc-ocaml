open Utils

let run file_name =
  let prog = Intcode.parse_input file_name in

  let m = Intcode.new_machine prog in
  let output = run_prog m 1 in

  match output with None -> 0 | Some out -> out
