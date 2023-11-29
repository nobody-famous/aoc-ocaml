let run lines =
  let data = Parser.parse_input lines in
  Printf.printf "min %d,%d\n" data.min_pt.x data.min_pt.y;
  Printf.printf "max %d,%d\n" data.max_pt.x data.max_pt.y;
  0
