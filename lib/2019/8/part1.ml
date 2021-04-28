let run file_name =
  let img = Parser.parse_input file_name 3 2 in
  Printf.printf "Layers %d\n" (List.length img);

  0
