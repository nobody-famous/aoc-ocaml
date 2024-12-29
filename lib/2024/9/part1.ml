let defrag (in_empty, in_files) =
  let rec do_defrag index empty files placed =
    match files with
    | file :: rest ->
        if index >= Array.length empty || file.Utils.block.pos < empty.(index).Utils.pos then
          List.append placed files
        else if empty.(index).size > 0 then (
          let file_size = file.block.size in
          let empty_size = empty.(index).size in

          if file_size > empty_size then
            let new_size = file_size - empty_size in
            let new_file = { file with block = { file.Utils.block with size = new_size } } in
            let placed_file = { Utils.id = file.id; block = empty.(index) } in
            do_defrag (index + 1) empty (new_file :: rest) (placed_file :: placed)
          else
            let new_size = empty_size - file_size in
            let placed_file = { Utils.id = file.id; block = { pos = empty.(index).pos; size = file_size } } in
            empty.(index) <- { Utils.pos = empty.(index).pos + file_size; size = new_size };
            do_defrag index empty rest (placed_file :: placed))
        else
          do_defrag (index + 1) empty files placed
    | [] -> List.append placed files
  in

  do_defrag 0 in_empty in_files []

let run lines = Aoc.Utils.IntResult (lines |> Parser.parse_input |> defrag |> Utils.checksum)
