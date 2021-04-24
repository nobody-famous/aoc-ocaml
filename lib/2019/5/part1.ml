let print_output num = print_endline (string_of_int num)

let run file_name =
  let prog = Intcode.parse_input file_name in
  let stdin = InputParser.read_lines "input/2019/5/stdin.txt" in
  let m = Intcode.new_machine_io prog stdin print_output in

  let _ = Intcode.run_prog m in

  0
