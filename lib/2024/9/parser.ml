let char_to_digit ch = Char.code ch - Char.code '0'
let string_to_seq str = str |> String.to_seq |> List.of_seq |> List.map char_to_digit
let calculate_disk_size input = List.fold_left (fun acc ch -> acc + ch) 0 input

let create_disk input =
  let rec do_create_disk is_file rem id pos disk =
    match rem with
    | n :: rest ->
        let new_is_file = not is_file in
        let new_pos = pos + n in
        let new_id = if is_file then id + 1 else id in

        if is_file then
          Array.fill disk pos n id;
        do_create_disk new_is_file rest new_id new_pos disk
    | [] -> disk
  in

  do_create_disk true input 0 0 (Array.make (calculate_disk_size input) (-1))

let build_blocks input =
  let rec do_build is_file next_id blocks_index (rem, blocks) =
    match rem with
    | n :: rest ->
        let new_is_file = not is_file in
        let new_id = if is_file then next_id + 1 else next_id in
        let new_block =
          if is_file then Utils.File { id = next_id; size = char_to_digit n } else Utils.Empty (char_to_digit n)
        in

        blocks.(blocks_index) <- new_block;
        do_build new_is_file new_id (blocks_index + 1) (rest, blocks)
    | [] -> blocks
  in

  let calculate_size input = (input, List.length input) in
  let build_array (input, size) = (input, Array.make size (Utils.Empty 0)) in

  input |> calculate_size |> build_array |> do_build true 0 0

(* let parse_input lines = lines |> List.hd |> string_to_seq |> build_blocks *)
let parse_input lines = lines |> List.hd |> string_to_seq |> create_disk
