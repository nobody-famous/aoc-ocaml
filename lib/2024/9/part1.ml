let next_empty_block start disk =
  let rec do_find index disk =
    if index >= Array.length disk then
      None
    else if disk.(index) = -1 then
      Some index
    else
      do_find (index + 1) disk
  in
  do_find start disk

let init_ptrs disk =
  let left_ptr = next_empty_block 0 disk in
  let right_ptr = Array.length disk - 1 in
  (left_ptr, right_ptr, disk)

let defrag (left, right, disk) =
  let rec do_defrag left_ptr right_ptr disk =
    match left_ptr with
    | Some ptr when ptr < right_ptr ->
        disk.(ptr) <- disk.(right_ptr);
        disk.(right_ptr) <- -1;
        do_defrag (next_empty_block ptr disk) (right_ptr - 1) disk
    | Some ptr when ptr >= right_ptr -> disk
    | _ -> disk
  in
  do_defrag left right disk

let checksum disk =
  disk |> Array.fold_left (fun (i, sum) v -> (i + 1, if v >= 0 then sum + (i * v) else sum)) (0, 0) |> fun (_, s) -> s

let run lines = Aoc.Utils.IntResult (lines |> Parser.parse_input |> init_ptrs |> defrag |> checksum)
