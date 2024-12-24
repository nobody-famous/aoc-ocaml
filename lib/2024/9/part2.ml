let run lines =
  let _ = lines |> Parser.parse_input in
  Aoc.Utils.IntResult 0
