open Parser

let solve grid =
  Printf.printf "%d\n" (Hashtbl.length grid.graph);
  0

let run lines = parse_input lines |> solve
