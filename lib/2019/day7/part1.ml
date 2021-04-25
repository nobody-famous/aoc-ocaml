let run_amp prog phase signal =
  let stdin = [ string_of_int phase; string_of_int signal ] in
  let output = ref 0 in

  let m =
    Intcode.new_machine_io (Array.copy prog) stdin (fun n -> output := n)
  in

  let _ = Intcode.run_prog m in

  !output

let run_seq prog seq =
  let rec loop seq signal =
    match seq with
    | [] -> signal
    | first :: rest -> loop rest (run_amp prog first signal)
  in

  loop seq 0

let run file_name =
  let prog = Intcode.parse_input file_name in

  let output = run_seq prog [ 1; 0; 4; 3; 2 ] in
  Printf.printf "output %d\n" output;

  output
