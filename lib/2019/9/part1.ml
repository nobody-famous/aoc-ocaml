let run file_name =
  let prog = Intcode.parse_input file_name in
  let mach = Intcode.new_machine prog in
  let mach = Intcode.set_input mach 1 in

  (* let mach = Intcode.set_debug mach true in *)
  let rec loop m =
    let m = Intcode.step m in

    match Intcode.get_state m with
    | HALT -> ()
    | RUN -> loop m
    | OUTPUT ->
        let m, out = Intcode.get_output m in
        Printf.printf "%d\n" (match out with Some v -> v | None -> 0);
        loop m
    | s -> Printf.printf "STATE %s\n" (Intcode.state_to_string s)
  in

  loop mach;
  0
