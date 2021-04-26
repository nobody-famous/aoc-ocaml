let run file_name =
  let prog = Intcode.parse_input file_name in

  let last_output = ref 0 in
  let update_last n = last_output := n in
  let stdin () = 1 in

  let m = Intcode.new_machine_io prog stdin update_last in
  let _ = Intcode.run_prog m in

  !last_output
