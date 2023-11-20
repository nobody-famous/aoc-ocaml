open Parser
module G = Aoc.Graph

let solve grid =
  G.shortest_path ~start_pos:grid.s ~end_pos:grid.e ~init_weight:0 grid.graph
  |> fun n -> n.weight

let run lines = parse_input lines |> solve
