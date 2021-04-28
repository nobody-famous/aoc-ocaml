open Utils

(* let start_machine m phase = run_to_input m phase *)

(* let start_machines ms phases =
  let rec loop ndx =
    if ndx < Array.length ms then (
      ms.(ndx) <- start_machine ms.(ndx) phases.(ndx);
      loop (ndx + 1))
  in

  loop 0 *)

(* let run_chain machines =
  let rec loop ndx out =
    if ndx < Array.length machines then (
      machines.(ndx) <- run_to_input machines.(ndx) out;

      let m, out' = run_to_output machines.(ndx) in
      machines.(ndx) <- m;

      loop (ndx + 1) out')
    else out
  in

  loop 0 0 *)

(* let run_perm prog perm =
  let machines =
    [|
      Intcode.new_machine prog;
      Intcode.new_machine prog;
      Intcode.new_machine prog;
      Intcode.new_machine prog;
      Intcode.new_machine prog;
    |]
  in

  start_machines machines perm;

  let rec loop n out =
       if n < 1 then (
         let out' = run_chain machines in
         Printf.printf "OUT %d\n" out';
         loop (n + 1) out')
       else out
     in

  loop 0 0
  0 *)

let run _ =
  (* let prog = Intcode.parse_input file_name in *)
  let _ = permutations [| 5; 6; 7; 8; 9 |] in
  (* let machines =
    [|
      Intcode.new_machine prog;
      Intcode.new_machine prog;
      Intcode.new_machine prog;
      Intcode.new_machine prog;
      Intcode.new_machine prog;
    |]
  in *)

  (* start_machines machines [| 9; 8; 7; 6; 5 |]; *)

  (* let _ = run_perm prog [| 9; 8; 7; 6; 5 |] in *)

  0
