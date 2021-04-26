open Utils

let run_amp prog phase signal =
  let stdin =
    let count = ref 0 in
    fun () ->
      let result = if !count = 0 then phase else signal in
      count := !count + 1;
      result
  in
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

let run_perms prog perms =
  List.fold_left
    (fun acc perm ->
      let result = run_seq prog perm in
      Stdlib.max result acc)
    0 perms

let run file_name =
  let prog = Intcode.parse_input file_name in
  let perms = permutations [| 0; 1; 2; 3; 4 |] in

  run_perms prog perms
