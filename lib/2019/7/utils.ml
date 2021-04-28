let swap a n1 n2 =
  let tmp = a.(n1) in
  a.(n1) <- a.(n2);
  a.(n2) <- tmp

let permutations amps =
  let rec loop size out =
    match size with
    | 1 -> Array.to_list amps :: out
    | size' ->
        let rec i_loop i stop out' =
          if i = stop then out'
          else
            let new_out = loop (size - 1) out' in

            if size mod 2 = 1 then swap amps 0 (size - 1)
            else swap amps i (size - 1);

            i_loop (i + 1) stop new_out
        in

        i_loop 0 size' out
  in

  loop (Array.length amps) []

let make_machines prog =
  [|
    Intcode.new_machine prog;
    Intcode.new_machine prog;
    Intcode.new_machine prog;
    Intcode.new_machine prog;
    Intcode.new_machine prog;
  |]

let start_machine machine phase =
  let rec loop m =
    let m = Intcode.step m in

    match Intcode.get_state m with
    | INPUT ->
        let m = Intcode.set_input m phase in
        m
    | HALT -> m
    | RUN -> loop m
  in

  loop machine

let start_machines machines phases =
  let rec loop ndx ph_list =
    match ph_list with
    | [] -> ()
    | first :: rest ->
        machines.(ndx) <- start_machine machines.(ndx) first;
        loop (ndx + 1) rest
  in

  loop 0 phases
