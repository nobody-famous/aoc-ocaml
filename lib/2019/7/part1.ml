open Utils

let run_amp prog phase signal =
  let rec loop m out first =
    let m = Intcode.step m in

    match Intcode.get_state m with
    | RUN ->
        let m, v = Intcode.get_output m in
        let out' = match v with None -> out | Some v -> v in

        loop m out' first
    | HALT -> out
    | INPUT ->
        let input = if first then phase else signal in
        let m = Intcode.set_input m input in
        loop m out false
  in

  let m = Intcode.new_machine prog in
  loop m signal true

let run_seq prog seq =
  let rec loop seq signal =
    match seq with
    | [] -> signal
    | first :: rest -> loop rest (run_amp prog first signal)
  in

  loop seq 0

(* let run_seq machines =
   let rec loop ndx signal =
     if ndx < Array.length machines then (
       let m = machines.(ndx) in
       let m, signal' = run_amp m signal in

       machines.(ndx) <- m;
       loop (ndx + 1) signal')
     else signal
   in

   loop 0 0 *)

let run_perms prog perms =
  List.fold_left
    (fun acc perm ->
      let machines = make_machines prog in
      start_machines machines perm;

      let result = run_seq prog perm in

      Stdlib.max result acc)
    0 perms

let run file_name =
  let prog = Intcode.parse_input file_name in
  let perms = permutations [| 0; 1; 2; 3; 4 |] in

  let v = run_perms prog perms in
  Printf.printf "%d\n" v;
  v
