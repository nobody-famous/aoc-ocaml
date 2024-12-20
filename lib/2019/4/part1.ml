open Parser
open Utils

let has_adj input =
  let rec loop ndx =
    if ndx >= Array.length input then
      false
    else if input.(ndx - 1) = input.(ndx) then
      true
    else
      loop (ndx + 1)
  in

  loop 1

let run lines = Aoc.Utils.IntResult (parse_input lines |> find_first |> count_pws has_adj)
