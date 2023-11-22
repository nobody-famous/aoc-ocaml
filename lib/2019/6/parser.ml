let parse_input lines =
  lines
  |> List.map (fun line -> String.split_on_char ')' line)
  |> List.map (fun s -> match s with h :: t :: _ -> (h, t) | _ -> ("", ""))
