open Utils

let run_amp machine signal =
  let rec loop m out =
    let m = Intcode.step m in

    match Intcode.get_state m with
    | RUN -> loop m out
    | HALT -> out
    | INPUT ->
        let m = Intcode.set_input signal m in
        loop m out
    | OUTPUT ->
        let m, v = Intcode.get_output m in
        let out' = match v with None -> out | Some v -> v in

        loop m out'
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
      prog |> make_machines |> start_machines perm |> run_seq |> Stdlib.max acc)
    0 perms

let run file_name =
  let prog = Intcode.parse_input file_name in
  let perms = permutations [| 0; 1; 2; 3; 4 |] in

  run_perms prog perms
