let next_empty_block disk = Array.find_index (fun item -> item = -1) disk

let init_ptrs disk =
  let left_ptr = next_empty_block disk in
  let right_ptr = Array.length disk - 1 in
  (left_ptr, right_ptr, disk)

let defrag (left, right, disk) =
  let rec do_defrag left_ptr right_ptr disk =
    match left_ptr with
    | Some ptr when ptr < right_ptr ->
        disk.(ptr) <- disk.(right_ptr);
        disk.(right_ptr) <- -1;
        do_defrag (next_empty_block disk) (right_ptr - 1) disk
    | Some ptr when ptr >= right_ptr -> disk
    | _ -> disk
  in
  do_defrag left right disk

let checksum disk =
  disk |> Array.fold_left (fun (i, sum) v -> (i + 1, if v >= 0 then sum + (i * v) else sum)) (0, 0) |> fun (_, s) -> s

let run lines = Aoc.Utils.IntResult (lines |> Parser.parse_input |> init_ptrs |> defrag |> checksum)
