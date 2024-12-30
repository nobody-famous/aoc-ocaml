let find_empty in_target in_empty =
  let rec do_find target index empty =
    if index >= Array.length empty || empty.(index).Utils.pos > target.Utils.block.pos then
      None
    else if target.block.size <= empty.(index).size then
      Some index
    else
      do_find target (index + 1) empty
  in
  do_find in_target 0 in_empty

let defrag (in_empty, in_files) =
  let rec do_defrag empty files result =
    match files with
    | file :: rest -> (
        match find_empty file empty with
        | Some index ->
            let empty_size = empty.(index).Utils.size in
            let file_size = file.Utils.block.size in
            let new_file = { file with block = { pos = empty.(index).pos; size = file_size } } in
            empty.(index) <- { pos = empty.(index).pos + file_size; size = empty_size - file_size };
            do_defrag empty rest (new_file :: result)
        | None -> do_defrag empty rest (file :: result))
    | [] -> result
  in

  do_defrag in_empty in_files []

let run lines = Aoc.Utils.IntResult (lines |> Parser.parse_input |> defrag |> Utils.checksum)
