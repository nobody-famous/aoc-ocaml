open AocUtils
open Utils

type state = {
  grid : pieces;
  grid_3d : (point_3d, int) Hashtbl.t;
  dists : (point, (point, graph_node) Hashtbl.t) Hashtbl.t;
  unvisited : (point, graph_node) Hashtbl.t;
  have_keys : int;
}

let new_state node grid dists =
  let state =
    {
      grid;
      grid_3d = Hashtbl.create 64;
      dists;
      unvisited = Hashtbl.create 64;
      have_keys = 0;
    }
  in
  Hashtbl.replace state.unvisited node.pt node;
  state

let keys_match node state = node.keys land state.have_keys = node.keys

let find_closest state =
  Hashtbl.fold
    (fun _ node acc ->
      if keys_match node state && node.dist < acc.dist then node else acc)
    state.unvisited
    { pt = { x = 0; y = 0 }; dist = Int.max_int; keys = 0 }

let find_to_visit node state =
  let candidates = Hashtbl.find state.dists node.pt in

  Hashtbl.iter
    (fun k v -> if keys_match v state then Hashtbl.replace state.unvisited k v)
    candidates;

  state

let visit node state =
  let mask =
    try Hashtbl.find state.grid.keys node.pt |> key_mask with Not_found -> 0
  in

  Hashtbl.remove state.unvisited node.pt;
  find_to_visit node { state with have_keys = state.have_keys lor mask }

let walk_map pt grid dists =
  let state = new_state { pt; dist = 0; keys = 0 } grid dists in

  let closest = find_closest state in
  let state = visit closest state in

  let closest = find_closest state in
  let state = visit closest state in

  let closest = find_closest state in
  let state = visit closest state in

  let closest = find_closest state in
  let state = visit closest state in

  Printf.printf "closest %d,%d\n" closest.pt.x closest.pt.y;
  Hashtbl.iter
    (fun _ v -> Printf.printf "  %d,%d\n" v.pt.x v.pt.y)
    state.unvisited

let run file_name =
  let pieces = Parser.parse_input file_name in
  let dists = DistMap.build_map pieces in

  walk_map pieces.enter pieces dists;

  0
