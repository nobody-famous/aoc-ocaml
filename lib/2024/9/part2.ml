let defrag input =
  let find_move blocks =
    let rec do_find_move left right blocks =
      if left > right then
        None
      else
        match (blocks.(left), blocks.(right)) with
        | _, Utils.Empty _ -> do_find_move left (right - 1) blocks
        | Utils.File _, _ -> do_find_move (left + 1) right blocks
        | Utils.Empty s, Utils.File f when s >= f.size -> Some (left, right)
        | Utils.Empty s, Utils.File f when s < f.size -> do_find_move (left + 1) right blocks
        | _ -> None
    in

    do_find_move 0 (Array.length blocks - 1) blocks
  in

  let do_defrag input =
    let move = find_move input in
    (match move with
    | Some (left, right) -> Printf.printf "***** FOUND MOVE %d <- %d\n" left right
    | None -> Printf.printf "***** NO MOVE FOUND\n");
    []
  in

  do_defrag input

let run lines =
  (* let _ = lines |> Parser.parse_input |> defrag |> Utils.checksum in *)
  Aoc.Utils.IntResult 0
