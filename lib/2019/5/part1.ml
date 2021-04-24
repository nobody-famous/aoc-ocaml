let run file_name =
  let prog = Intcode.parse_input file_name in
  let m = Intcode.new_machine prog in

  let _ = Intcode.run_prog m in

  0
