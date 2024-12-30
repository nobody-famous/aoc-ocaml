let char_to_digit ch = Char.code ch - Char.code '0'
let string_to_seq str = str |> String.to_seq |> List.of_seq |> List.map char_to_digit

let build_blocks input =
  let rec do_build is_file next_id pos (empty, files) rem =
    match rem with
    | n :: rest ->
        let new_is_file = not is_file in
        let new_id = if is_file then next_id + 1 else next_id in
        let new_empty = if is_file then empty else { Utils.pos; size = n } :: empty in
        let new_files = if is_file then { Utils.id = next_id; block = { pos; size = n } } :: files else files in

        do_build new_is_file new_id (pos + n) (new_empty, new_files) rest
    | [] -> (Array.of_list (List.rev empty), files)
  in

  input |> do_build true 0 0 ([], [])

let parse_input lines = lines |> List.hd |> string_to_seq |> build_blocks
