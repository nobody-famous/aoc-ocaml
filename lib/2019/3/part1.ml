open Parser
open Utils
open Types

let run file_name =
  let wire1, wire2 = parse_input file_name in
  let colls = collisions wire1 wire2 in
  let dists = List.map (fun c -> man_dist { x = 0; y = 0 } c) colls in

  List.fold_left Stdlib.min Int.max_int dists
