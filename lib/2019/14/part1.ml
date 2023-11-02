open Utils

let run lines = Parser.parse_input lines |> calc_ore 1
