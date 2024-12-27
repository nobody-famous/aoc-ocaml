let init_ptrs blocks = (0, Array.length blocks - 1, blocks)

let checksum input =
  (* let sum_range first last = List.init (last - first + 1) (fun i -> i + first) |> List.fold_left ( + ) 0 in *)
  let sum_range first last =
    let first_float = float_of_int first in
    let last_float = float_of_int last in
    int_of_float @@ ((first_float +. last_float) /. 2. *. (last_float -. first_float +. 1.))
  in

  let rec do_checksum pos blocks total =
    match blocks with
    | block :: rest -> (
        match block with
        | Parser.Empty s -> do_checksum (pos + s) rest total
        | Parser.File f -> do_checksum (pos + f.size) rest (total + (f.id * sum_range pos (pos + f.size - 1))))
    | _ -> total
  in
  do_checksum 0 input 0

let defrag (left_ptr, right_ptr, input) =
  let rec do_defrag left right input output =
    if left > right then
      output
    else
      match (input.(left), input.(right)) with
      | Parser.File _, _ -> do_defrag (left + 1) right input (input.(left) :: output)
      | _, Parser.Empty _ -> do_defrag left (right - 1) input output
      | Parser.Empty s, Parser.File f ->
          if s < f.size then (
            let new_block = Parser.File { id = f.id; size = s } in

            input.(right) <- Parser.File { id = f.id; size = f.size - s };
            do_defrag (left + 1) right input (new_block :: output))
          else if s > f.size then (
            input.(left) <- Parser.Empty (s - f.size);
            do_defrag left (right - 1) input (input.(right) :: output))
          else
            do_defrag (left + 1) (right - 1) input (input.(right) :: output)
  in

  do_defrag left_ptr right_ptr input [] |> List.rev

let run lines = Aoc.Utils.IntResult (lines |> Parser.parse_input |> init_ptrs |> defrag |> checksum)
