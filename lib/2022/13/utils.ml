type packet = Integer of int | PacketList of packet list
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
  | PacketList list_1, PacketList list_2 -> cmp_lists list_1 list_2
  | PacketList list_1, Integer int_2 -> cmp_lists list_1 [ Integer int_2 ]
  | Integer int_1, PacketList list_2 -> cmp_lists [ Integer int_1 ] list_2
  | Integer left, Integer right ->
      if left < right then InOrder
      else if right < left then OutOfOrder
      else Unknown