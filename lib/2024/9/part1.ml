let defrag (left_prt, right_ptr, in_disk) =
  let rec do_defrag left right disk =
    if left > right then
      disk
    else if disk.(right) = -1 then
      do_defrag left (right - 1) disk
    else if disk.(left) = -1 then (
      disk.(left) <- disk.(right);
      disk.(right) <- -1;
      do_defrag (left + 1) (right - 1) disk)
    else
      do_defrag (left + 1) right disk
  in

  do_defrag left_prt right_ptr in_disk

let run lines = Aoc.Utils.IntResult (lines |> Parser.parse_input |> Utils.init_ptrs |> defrag |> Utils.checksum)
