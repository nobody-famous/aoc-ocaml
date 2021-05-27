open AocUtils
open Utils

type state = { unvisited : (point, graph_node) Hashtbl.t; have_keys : int }

let new_state node =
  let state = { unvisited = Hashtbl.create 64; have_keys = 0 } in
  Hashtbl.replace state.unvisited node.pt node;
  state

let keys_match node state = node.keys land state.have_keys = node.keys

let find_closest state =
  Hashtbl.fold
    (fun _ node acc ->
      if keys_match node state && node.dist < acc.dist then node else acc)
    state.unvisited
    { pt = { x = 0; y = 0 }; dist = Int.max_int; keys = 0 }

let walk_map pt dists =
  let node_dists = Hashtbl.find dists pt in
  let state = new_state { pt; dist = 0; keys = 0 } in
  let closest = find_closest state in

  Hashtbl.iter (fun k v -> Printf.printf "%d,%d %d\n" k.x k.y v.dist) node_dists;
  Printf.printf "closest %d,%d\n" closest.pt.x closest.pt.y

let run file_name =
  let pieces = Parser.parse_input file_name in
  let dists = DistMap.build_map pieces in

  walk_map pieces.enter dists;

  0
