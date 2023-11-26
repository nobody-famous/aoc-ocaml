let char_to_int ch = Char.code ch - Char.code '0'

let parse_num chars =
  let rec loop acc chars =
    match chars with
    | ch :: rest when ch >= '0' && ch <= '9' ->
        loop ((acc * 10) + char_to_int ch) rest
    | _ -> (chars, acc)
  in

  loop 0 chars

let unwrap_list item =
  match item with
  | Utils.PacketList pkt -> pkt
  | Utils.Integer _ -> failwith "Found unexpected integer"

let collapse stack =
  match stack with
  | fst :: snd :: rem ->
      let fst_pkt = unwrap_list fst in
      let snd_pkt = unwrap_list snd in
      Utils.PacketList (Utils.PacketList (List.rev fst_pkt) :: snd_pkt) :: rem
  | top :: rem ->
      let top_pkt = unwrap_list top in
      Utils.PacketList (List.rev top_pkt) :: rem
  | _ -> stack

let push num stack =
  match stack with
  | hd :: rest ->
      let hd_pkt = unwrap_list hd in
      Utils.PacketList (Utils.Integer num :: hd_pkt) :: rest
  | _ -> failwith "Push given empty stack"

let rec parse_list stack chars =
  match chars with
  | '[' :: rem -> parse_list (Utils.PacketList [] :: stack) rem
  | ']' :: rem -> parse_list (collapse stack) rem
  | '0' .. '9' :: _ ->
      let rem, num = parse_num chars in
      parse_list (push num stack) rem
  | ',' :: rem -> parse_list stack rem
  | _ -> (chars, List.hd stack)

let chars_to_pkt chars =
  match chars with
  | '[' :: _ -> parse_list [] chars
  | _ -> failwith "Invalid packet"

let parse_line (line : string) =
  line |> String.to_seq |> List.of_seq |> chars_to_pkt

let group_lines lines =
  let proc_line acc line =
    match acc with
    | hd :: rest -> (
        match line with
        | "" -> [] :: hd :: rest
        | _ -> (parse_line line :: hd) :: rest)
    | _ -> acc
  in

  List.fold_left proc_line [ [] ] lines |> List.map List.rev |> List.rev

let parse_input lines = group_lines lines
