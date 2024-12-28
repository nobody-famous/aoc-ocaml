let defrag (left_ptr, right_ptr, in_disk) =
  let rec find_next_file stop pos size target disk =
    if pos <= stop then
      None
    else if target = -1 then
      if disk.(pos) = -1 then
        find_next_file stop (pos - 1) 0 target disk
      else
        find_next_file stop (pos - 1) 1 disk.(pos) disk
    else if disk.(pos) = target then
      find_next_file stop (pos - 1) (size + 1) target disk
    else
      Some (pos + 1, size)
  in

  let rec find_next_empty stop pos size disk =
    if pos >= Array.length disk || pos >= stop then
      None
    else if disk.(pos) != -1 then
      if size = 0 then
        find_next_empty stop (pos + 1) 0 disk
      else
        Some (pos - size, size)
    else
      find_next_empty stop (pos + 1) (size + 1) disk
  in

  let print_disk disk =
    Printf.printf "****** DISK\n";
    Array.iter (fun n -> Printf.printf "%c" @@ if n >= 0 then char_of_int (n + Char.code '0') else '_') disk;
    Printf.printf "\n******\n"
  in

  let rec do_defrag left right disk =
    Printf.printf "***** DO DEFRAG %d %d\n" left right;
    match (find_next_empty right left 0 disk, find_next_file left right 0 (-1) disk) with
    | Some (l, l_size), Some (r, r_size) when l_size >= r_size ->
        Array.fill disk l r_size disk.(r);
        Array.fill disk r r_size (-1);
        Printf.printf "***** NEXT FILE %d %d -> %d %d\n" r r_size l l_size;
        print_disk disk;
        do_defrag (l + l_size) (r - 1) disk
    | Some (l, l_size), Some (r, r_size) when l_size < r_size -> do_defrag (l + l_size) (r + r_size - 1) disk
    | None, Some (r, r_size) ->
        Printf.printf "***** NO EMPTY SPACE %d %d\n" r r_size;
        disk
    | None, None -> do_defrag 0 (right - 1) disk
    | _ -> disk
  in

  do_defrag left_ptr right_ptr in_disk

let run lines = Aoc.Utils.IntResult (lines |> Parser.parse_input |> Utils.init_ptrs |> defrag |> Utils.checksum)
