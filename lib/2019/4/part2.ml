open Parser
open Utils

let has_double counts =
  let rec loop ndx =
    if ndx >= Array.length counts then false
    else if counts.(ndx) = 2 then true
    else loop (ndx + 1)
  in

  loop 0

let is_valid input =
  let counts = Array.make 10 0 in
  let rec loop ndx =
    if ndx >= Array.length input then has_double counts
    else
      let n = input.(ndx) in
      let c = counts.(n) in

      counts.(n) <- c + 1;
      loop (ndx + 1)
  in

  loop 0

let run file_name = parse_input file_name |> find_first |> count_pws is_valid
