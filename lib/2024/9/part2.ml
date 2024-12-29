let defrag (_, right_ptr, in_disk) =
  let find_next_file start_pos in_target in_disk =
    let rec do_find_next pos size target disk =
      if pos < 0 then
        None
      else if disk.(pos) = target then
        do_find_next (pos - 1) (size + 1) target disk
      else if size > 0 then
        Some (pos + 1, size)
      else
        do_find_next (pos - 1) 0 target disk
    in

    do_find_next start_pos 0 in_target in_disk
  in

  let find_next_empty end_pos target_size disk =
    let rec do_find_next stop pos size target_size disk =
      if pos >= Array.length disk || pos >= stop then
        if size >= target_size then
          Some (pos - size)
        else
          None
      else if disk.(pos) != -1 then
        if size < target_size then
          do_find_next stop (pos + 1) 0 target_size disk
        else
          Some (pos - size)
      else
        do_find_next stop (pos + 1) (size + 1) target_size disk
    in

    do_find_next end_pos 0 0 target_size disk
  in

  let rec do_defrag right target disk =
    let file = find_next_file right target disk in

    match file with
    | None ->
        if target = 0 then
          disk
        else
          do_defrag right (target - 1) disk
    | Some (file_pos, size) -> (
        let empty = find_next_empty file_pos size disk in
        match empty with
        | None -> do_defrag (file_pos - 1) (target - 1) disk
        | Some empty_pos ->
            Array.fill disk empty_pos size disk.(file_pos);
            Array.fill disk file_pos size (-1);
            do_defrag (file_pos - 1) (target - 1) disk)
  in

  do_defrag right_ptr in_disk.(right_ptr) in_disk

(* let run lines = Aoc.Utils.IntResult (lines |> Parser.parse_input |> Utils.init_ptrs |> defrag |> Utils.checksum) *)
let run lines = Aoc.Utils.IntResult 0
