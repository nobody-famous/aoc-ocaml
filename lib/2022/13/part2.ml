let div_1 = Utils.PacketList [ Utils.PacketList [ Utils.Integer 2 ] ]
let div_2 = Utils.PacketList [ Utils.PacketList [ Utils.Integer 6 ] ]

let collapse_groups grps = let break_pair (left, right) = [ left; right ] in

                           grps |> List.map break_pair |> List.flatten

let add_dividers packets = div_1 :: div_2 :: packets

let cmp_packets pkt_1 pkt_2 =
  match Utils.in_order (pkt_1, pkt_2) with
  | Utils.InOrder -> -1
  | Utils.OutOfOrder -> 1
  | Utils.Unknown -> 0

let to_indices index pkt = if pkt = div_1 || pkt = div_2 then index + 1 else 1

let run lines =
  Aoc.Utils.IntResult
    (lines
    |> Parser.parse_input
    |> collapse_groups
    |> add_dividers
    |> List.sort cmp_packets
    |> List.mapi to_indices
    |> List.fold_left ( * ) 1)
