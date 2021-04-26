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
