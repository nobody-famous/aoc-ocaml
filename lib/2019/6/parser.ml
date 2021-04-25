let parse_input file_name =
  InputParser.read_lines file_name
  |> List.map (fun line -> String.split_on_char ')' line)
  |> List.map (fun s -> match s with h :: t :: _ -> (h, t) | _ -> ("", ""))
