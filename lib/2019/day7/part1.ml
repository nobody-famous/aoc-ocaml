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

let swap a n1 n2 =
  let tmp = a.(n1) in
  a.(n1) <- a.(n2);
  a.(n2) <- tmp

let perms amps =
  let rec loop size out =
    match size with
    | 1 -> Array.copy amps :: out
    | size' ->
        let rec i_loop i stop out' =
          if i = stop then out'
          else
            let o = loop (size - 1) out' in

            if size mod 2 = 1 then swap amps 0 (size - 1)
            else swap amps i (size - 1);

            i_loop (i + 1) stop o
        in

        i_loop 0 size' out
  in

  loop (Array.length amps) []

let run file_name =
  let prog = Intcode.parse_input file_name in

  let p = perms [| 0; 1; 2; 3; 4 |] in

  Printf.printf "perms %d\n" (List.length p);

  let output = run_seq prog [ 1; 0; 4; 3; 2 ] in
  Printf.printf "output %d\n" output;

  output
