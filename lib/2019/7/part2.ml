open Utils

let run_to_output m =
  let output = ref None in
  let m = Intcode.set_stdout m (fun n -> output := Some n) in

  let rec loop m =
    let m = Intcode.step m in
    if Intcode.halted m then None
    else match !output with None -> loop m | Some v -> Some v
  in

  match loop m with Some v -> (m, v) | None -> raise (Failure "NO OUTPUT")

let run_to_input m input =
  let has_input = ref false in
  let m =
    Intcode.set_stdin m (fun () ->
        has_input := true;
        input)
  in

  let rec loop m =
    let m = Intcode.step m in
    if !has_input then m else loop m
  in

  loop m

let start_machine m phase = run_to_input m phase

let start_machines ms phases =
  let rec loop ndx =
    if ndx >= Array.length ms then ms
    else (
      ms.(ndx) <- start_machine ms.(ndx) phases.(ndx);
      loop (ndx + 1))
  in

  loop 0

let run file_name =
  let prog = Intcode.parse_input file_name in
  let _ = permutations [| 5; 6; 7; 8; 9 |] in
  let machines =
    [|
      Intcode.new_machine prog;
      Intcode.new_machine prog;
      Intcode.new_machine prog;
      Intcode.new_machine prog;
      Intcode.new_machine prog;
    |]
  in

  let machines = start_machines machines [| 9; 8; 7; 6; 5 |] in
  let _, v = run_to_output machines.(0) in

  Printf.printf "OUTPUT %d\n" v;

  0
