open Utils

let run file_name = Parser.parse_input file_name |> calc_ore 1
