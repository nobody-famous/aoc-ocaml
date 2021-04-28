open Utils

let run_amp machine signal =
  let rec loop m out =
    let m = Intcode.step m in

    match Intcode.get_state m with
    | RUN ->
        let m, v = Intcode.get_output m in
        let out' = match v with None -> out | Some v -> v in

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
  List.fold_left
    (fun acc perm ->
      let machines = make_machines prog in
      start_machines machines perm;

      let result = run_seq machines in

      Stdlib.max result acc)
    0 perms

let run file_name =
  let prog = Intcode.parse_input file_name in
  let perms = permutations [| 0; 1; 2; 3; 4 |] in

  run_perms prog perms
