open Parser
open Utils
open Types

let run lines =
  parse_input lines
  |> collisions
  |> List.map (fun c -> man_dist { x = 0; y = 0 } c)
  |> List.fold_left Stdlib.min Int.max_int
