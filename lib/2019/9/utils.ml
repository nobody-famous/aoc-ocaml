let rec run_mach out m =
  let m = Intcode.step m in

  match Intcode.get_state m with
  | HALT -> out
  | RUN -> run_mach out m
  | OUTPUT -> (
      let m, out' = Intcode.get_output m in
      match out' with Some v -> v | None -> run_mach 0 m)
  | s ->
      Printf.printf "STATE %s\n" (Intcode.state_to_string s);
      0
