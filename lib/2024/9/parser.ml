type block = File of { id : int; size : int } | Empty of int

let to_tuple x y = (x, y)
let char_to_digit ch = Char.code ch - Char.code '0'
let calculate_disk_size line = line |> List.fold_left (fun acc ch -> acc + ch) 0 |> to_tuple line
let create_disk (line, size) = (line, Array.make size (-1))

let populate_disk (line, disk) =
  let rec do_populate is_file offset id rem disk =
    match rem with
    | n :: rest ->
        let new_is_file = not is_file in
        let new_offset = offset + n in
        let new_id = if is_file then id + 1 else id in
        let fill = if is_file then id else -1 in

        Array.fill disk offset n fill;
        do_populate new_is_file new_offset new_id rest disk
    | [] -> disk
  in
  do_populate true 0 0 line disk

let build_disk line =
  line |> String.to_seq |> List.of_seq |> List.map char_to_digit |> calculate_disk_size |> create_disk |> populate_disk

let build_blocks lines =
  let rec do_build is_file next_id blocks rem =
    match rem with
    | n :: rest ->
        let new_is_file = not is_file in
        let new_id = if is_file then next_id + 1 else next_id in
        let new_block = if is_file then File { id = next_id; size = char_to_digit n } else Empty (char_to_digit n) in

        do_build new_is_file new_id (new_block :: blocks) rest
    | [] -> blocks
  in

  lines |> List.hd |> String.to_seq |> List.of_seq |> do_build true 0 [] |> List.rev

(* let parse_input lines = lines |> List.hd |> build_disk *)
let parse_input lines = lines |> build_blocks
