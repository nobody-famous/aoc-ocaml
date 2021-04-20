open Parser
open Printf

let run file_name =
  let prog = parse_input file_name in

  let print_array a =
    let rec helper ndx =
      match ndx with
      | n when ndx < Array.length a ->
          printf ",%d" a.(n);
          helper (n + 1)
      | _ -> printf "\n"
    in

    match Array.length a with
    | 0 -> ()
    | _ ->
        printf "%d" a.(0);
        helper 1
  in

  print_array prog;
  let ndx = 3 in
  let a, b, c = (prog.(ndx), prog.(ndx + 1), prog.(ndx + 2)) in
  printf "%d,%d,%d\n" a b c;
  0
