type order_result = InOrder | OutOfOrder | Unknown

let rec in_order (pkt_1, pkt_2) =
  let rec cmp_lists list_1 list_2 =
    match (list_1, list_2) with
    | fst_1 :: rest_1, fst_2 :: rest_2 -> (
        match in_order (fst_1, fst_2) with
        | InOrder -> InOrder
        | OutOfOrder -> OutOfOrder
        | Unknown -> cmp_lists rest_1 rest_2)
    | [], [] -> Unknown
    | [], _ -> InOrder
    | _, [] -> OutOfOrder
  in

  match (pkt_1, pkt_2) with
  | Utils.PacketList list_1, Utils.PacketList list_2 -> cmp_lists list_1 list_2
  | Utils.PacketList list_1, Utils.Integer int_2 ->
      cmp_lists list_1 [ Utils.Integer int_2 ]
  | Utils.Integer int_1, Utils.PacketList list_2 ->
      cmp_lists [ Utils.Integer int_1 ] list_2
  | Utils.Integer left, Utils.Integer right ->
      if left < right then InOrder
      else if right < left then OutOfOrder
      else Unknown

let to_indices index result =
  match result with
  | InOrder -> index + 1
  | OutOfOrder -> 0
  | Unknown -> 0

let run lines =
  lines
  |> Parser.parse_input
  |> List.map in_order
  |> List.mapi to_indices
  |> List.fold_left ( + ) 0
