let defrag (left_ptr, right_ptr, input) =
  let rec do_defrag left right input output =
    if left > right then
      output
    else
      match (input.(left), input.(right)) with
      | Utils.File _, _ -> do_defrag (left + 1) right input (input.(left) :: output)
      | _, Utils.Empty _ -> do_defrag left (right - 1) input output
      | Utils.Empty s, Utils.File f ->
          if s < f.size then (
            let new_block = Utils.File { id = f.id; size = s } in

            input.(right) <- Utils.File { id = f.id; size = f.size - s };
            do_defrag (left + 1) right input (new_block :: output))
          else if s > f.size then (
            input.(left) <- Utils.Empty (s - f.size);
            do_defrag left (right - 1) input (input.(right) :: output))
          else
            do_defrag (left + 1) (right - 1) input (input.(right) :: output)
  in

  do_defrag left_ptr right_ptr input [] |> List.rev

let run lines = Aoc.Utils.IntResult (lines |> Parser.parse_input |> Utils.init_ptrs |> defrag |> Utils.checksum)
