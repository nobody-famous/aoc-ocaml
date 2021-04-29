let run file_name =
  let prog = Intcode.parse_input file_name in
  let _ = Intcode.new_machine prog in
  (* let mach = Intcode.set_debug mach true in *)

  0
