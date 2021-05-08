open Utils

let run_amp machine signal =
  let rec loop m out =
    let m = Intcode.step m in

    match Intcode.get_state m with
    | RUN -> loop m out
    | OUTPUT ->
        let m, v = Intcode.get_output m in
        let out' = match v with None -> out | Some v -> v in

        (m, out')
    | HALT -> (m, out)
    | INPUT ->
        let m = Intcode.set_input signal m in
        loop m out
  in

  loop machine signal

let run_seq machines =
  let rec loop ndx signal out =
    let next = if ndx + 1 < Array.length machines then ndx + 1 else 0 in
    let m = machines.(ndx) in
    let m, signal' = run_amp m signal in
    let out' = if ndx = Array.length machines - 1 then signal' else out in

    machines.(ndx) <- m;

    if Intcode.get_state m = HALT && ndx = Array.length machines - 1 then out
    else loop next signal' out'
  in

  loop 0 0 0

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
  let perms = permutations [| 9; 8; 7; 6; 5 |] in

  run_perms prog perms
