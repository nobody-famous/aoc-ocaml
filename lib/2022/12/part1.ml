open Parser
open Utils
module G = Aoc.Graph

let inc_char ch = ch |> Char.code |> ( + ) 1 |> Char.chr

let to_edge cells ch neighbor_pos =
  match Hashtbl.find_opt cells neighbor_pos with
  | Some neighbor_ch ->
      if neighbor_ch <= inc_char ch then
        Some { G.target = neighbor_pos; G.weight = 1 }
      else None
  | None -> None

let solve grid =
  grid.graph
  |> G.shortest_path
       {
         start_pos = grid.s;
         initial_weight = 0;
         is_end = (fun p -> p = grid.e);
       }
  |> fun n -> n.weight

let run lines = parse_input lines |> build_grid to_edge |> solve
