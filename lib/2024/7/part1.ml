let get_answer _ = Aoc.Utils.IntResult 0
let run lines = lines |> Parser.parse_input |> get_answer
