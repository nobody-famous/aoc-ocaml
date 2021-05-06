open Utils

let run file_name =
  let prog = Intcode.parse_input file_name in
  let mach = Intcode.new_machine () prog in
  let mach = Intcode.set_input mach 2 in

  run_mach mach 0
