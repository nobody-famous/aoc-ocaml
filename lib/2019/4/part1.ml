open Printf
open Parser

let find_first input =
  let first = Array.make (Array.length input) input.(0) in
  let rec loop ndx =
    match ndx with
    | _ when ndx >= Array.length input -> ()
    | n ->
        if first.(n - 1) < input.(n) then first.(n) <- input.(n)
        else first.(n) <- first.(n - 1);
        loop (n + 1)
  in

  loop 1;
  first

let reset input ndx value =
  let rec loop n =
    if n < Array.length input then (
      input.(n) <- value;
      loop (n + 1))
    else ()
  in

  loop ndx

let inc input =
  let rec loop ndx =
    match ndx with
    | _ when ndx < 0 -> ()
    | _ -> (
        match input.(ndx) + 1 with
        | v when v = 10 ->
            input.(ndx) <- 0;
            loop (ndx - 1)
        | v ->
            input.(ndx) <- v;
            reset input (ndx + 1) v)
  in

  loop (Array.length input - 1)

let run file_name =
  let first, _ = parse_input file_name in

  let print_array a =
    let rec loop ndx =
      match ndx with
      | n when ndx < Array.length a ->
          printf "%d" a.(n);
          loop (ndx + 1)
      | _ -> ()
    in

    loop 0;
    printf "\n"
  in

  let next = find_first first in
  inc next;

  printf "Next ";
  print_array next
