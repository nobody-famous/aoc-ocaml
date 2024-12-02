let to_indices index result =
  match result with
  | Utils.InOrder -> index + 1
  | Utils.OutOfOrder -> 0
  | Utils.Unknown -> 0

let run lines =
  Aoc.Utils.IntResult
    (lines |> Parser.parse_input |> List.map Utils.in_order |> List.mapi to_indices |> List.fold_left ( + ) 0)
