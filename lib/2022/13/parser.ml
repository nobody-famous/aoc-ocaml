let char_to_int ch = Char.code ch - Char.code '0'

let parse_num chars =
  let rec loop acc chars =
    match chars with
    | ch :: rest when ch >= '0' && ch <= '9' ->
        loop ((acc * 10) + char_to_int ch) rest
    | _ -> (chars, acc)
  in

  loop 0 chars

let collapse stack =
  match stack with
  | fst :: snd :: rem -> (
      match snd with
      | Utils.PacketList pkt -> Utils.PacketList (fst :: pkt) :: rem
      | Utils.Integer _ -> failwith "Collapse found an integer on the stack")
  | hd -> (
      match hd with
      | top :: rem -> (
          match top with
          | Utils.PacketList pkt -> Utils.PacketList (List.rev pkt) :: rem
          | Utils.Integer _ -> hd)
      | _ -> hd)

let push num stack =
  match stack with
  | hd :: rest -> (
      match hd with
      | Utils.PacketList pkt ->
          Utils.PacketList (Utils.Integer num :: pkt) :: rest
      | Utils.Integer _ -> failwith "Push top of stack is integer")
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
