open Utils

let run lines = Aoc.Utils.IntResult (Parser.parse_input lines |> calc_ore 1)
