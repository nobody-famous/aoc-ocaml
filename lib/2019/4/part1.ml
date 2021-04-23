open Printf
open Parser

let find_first input =
  let next = Array.make (Array.length input) input.(0) in
  let rec helper ndx =
    match ndx with
    | _ when ndx >= Array.length input -> ()
    | n when next.(ndx - 1) < input.(ndx) ->
        next.(n) <- input.(n);
        helper (n + 1)
    | n ->
        next.(n) <- next.(n - 1);
        helper (n + 1)
  in

  helper 1;
  next

let run file_name =
  let first, _ = parse_input file_name in

  let print_array a =
    let rec helper ndx =
      match ndx with
      | n when ndx < Array.length a ->
          printf "%d" a.(n);
          helper (ndx + 1)
      | _ -> ()
    in

    helper 0;
    printf "\n"
  in

  let next = find_first first in
  printf "Next ";
  print_array next
