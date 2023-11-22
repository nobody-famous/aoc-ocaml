let char_to_int ch = Char.code ch - Char.code '0'

let parse_num chars =
  let rec loop acc chars =
    match chars with
    | ch :: rest when ch >= '0' && ch <= '9' ->
        loop ((acc * 10) + char_to_int ch) rest
    | _ -> (chars, acc)
  in

  loop 0 chars

let chars_to_pkt chars =
  let rec parse_list chars =
    match chars with
    | '[' :: rest -> parse_list rest
    | '0' .. '9' :: _ -> parse_num chars
    | _ -> (chars, 0)
  in

  match chars with
  | '[' :: rest -> parse_list rest
  | _ -> failwith "Invalid packet"

let parse_line (line : string) =
  line |> String.to_seq |> List.of_seq |> chars_to_pkt

let group_lines lines =
  List.fold_left
    (fun acc line ->
      match acc with
      | hd :: rest -> (
          match line with
          | "" -> [] :: hd :: rest
          | _ -> (parse_line line :: hd) :: rest)
      | _ -> acc)
    [ [] ] lines
  |> List.map List.rev
  |> List.rev

let parse_input lines = let _ = group_lines lines in

                        0
