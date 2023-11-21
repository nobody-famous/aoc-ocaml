open Parser
open Utils
module G = Aoc.Graph

let dec_char ch =
  match ch with
  | 'a' -> ch
  | _ -> ch |> Char.code |> fun c -> c - 1 |> Char.chr

let to_edge cells ch neighbor_pos =
  match Hashtbl.find_opt cells neighbor_pos with
  | Some neighbor_ch ->
      if neighbor_ch >= dec_char ch then
        Some { G.target = neighbor_pos; G.weight = 1 }
      else None
  | None -> None

let solve grid =
  G.shortest_path ~start_pos:grid.s ~end_pos:grid.e ~init_weight:0 grid.graph
  |> fun n -> n.weight

let run lines = parse_input lines |> Utils.build_grid to_edge |> solve
