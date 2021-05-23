let find_first (input, last) =
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
  (first, last)

let is_greater a b =
  let rec loop ndx =
    if ndx >= Array.length a then true
    else if a.(ndx) = b.(ndx) then loop (ndx + 1)
    else a.(ndx) > b.(ndx)
  in

  loop 0

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

let count_pws is_valid (first, last) =
  let copy = Array.copy first in
  let rec loop count =
    if is_greater copy last then count
    else (
      inc copy;
      loop (if is_valid copy then count + 1 else count))
  in

  loop 0
