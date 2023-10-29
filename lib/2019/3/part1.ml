open Parser
open Utils
open Types

let run file_name =
  parse_input file_name |> collisions
  |> List.map (fun c -> man_dist { x = 0; y = 0 } c)
  |> List.fold_left Stdlib.min Int.max_int
