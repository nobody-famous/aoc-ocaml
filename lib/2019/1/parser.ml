let parse_input (file_name : string) : int list =
  InputParser.read_lines file_name |> List.map int_of_string
