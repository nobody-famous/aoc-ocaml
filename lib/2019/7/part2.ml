open Utils

let run_amp machine signal =
  let rec loop m out =
    let m = Intcode.step m in

    match Intcode.get_state m with
    | RUN ->
        let m, v = Intcode.get_output m in
        let out' = match v with None -> out | Some v -> v in

        Printf.printf "out %d\n" out';
        loop m out'
    | HALT -> out
    | INPUT ->
        let m = Intcode.set_input m signal in
        loop m out
  in

  loop machine signal

let run_seq machines =
  let rec loop ndx signal =
    if ndx < Array.length machines then (
      let m = machines.(ndx) in
      let signal' = run_amp m signal in

      machines.(ndx) <- m;
      loop (ndx + 1) signal')
    else signal
  in

  loop 0 0

let run_perms prog perms =
  let machines = make_machines prog in

  match perms with
  | first :: _ ->
      start_machines machines first;
      Printf.printf "after start\n";
      let signal = run_seq machines in
      Printf.printf "signal %d\n" signal
  | _ -> ()

(* List.fold_left
   (fun acc perm ->
     let machines = make_machines prog in
     Printf.printf "START\n";
     start_machines machines perm;

     let result = run_seq machines in

     Stdlib.max result acc)
   0 perms *)

let run file_name =
  let prog = Intcode.parse_input file_name in
  let perms = permutations [| 5; 6; 7; 8; 9 |] in

  run_perms prog perms
