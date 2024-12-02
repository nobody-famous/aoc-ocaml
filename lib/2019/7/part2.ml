open Utils

let run_amp signal machine =
  let rec loop m out =
    let m = Intcode.step m in

    match Intcode.get_state m with
    | Run -> loop m out
    | HasOutput ->
        let m, v = Intcode.get_output m in
        let out' =
          match v with
          | None -> out
          | Some v -> v
        in

        (m, out')
    | Halt -> (m, out)
    | NeedInput ->
        let m = Intcode.set_input signal m in
        loop m out
  in

  loop machine signal

let run_seq machines =
  let rec loop ndx signal out =
    let next = if ndx + 1 < Array.length machines then ndx + 1 else 0 in
    let m, signal' = machines.(ndx) |> run_amp signal in
    let out' = if ndx = Array.length machines - 1 then signal' else out in

    machines.(ndx) <- m;

    if Intcode.get_state m = Halt && ndx = Array.length machines - 1 then
      out
    else
      loop next signal' out'
  in

  loop 0 0 0

let run_perms prog perms =
  List.fold_left (fun acc perm -> prog |> make_machines |> start_machines perm |> run_seq |> Stdlib.max acc) 0 perms

let run lines =
  let prog = Intcode.parse_input lines in
  let perms = permutations [| 9; 8; 7; 6; 5 |] in

  Aoc.Utils.IntResult (run_perms prog perms)
