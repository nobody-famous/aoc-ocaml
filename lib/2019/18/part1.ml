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

let keys_match node state =
  (node.keys = 0 && state.have_keys = 0)
  || node.keys land state.have_keys = node.keys

let find_closest state =
  Hashtbl.fold
    (fun _ node acc ->
      if keys_match node state && node.dist < acc.dist then node else acc)
    state.unvisited
    { pt = { x = 0; y = 0 }; dist = Int.max_int; keys = 0 }

let is_closer pt_3d dist state =
  let cur_dist =
    try Hashtbl.find state.grid_3d pt_3d with Not_found -> Int.max_int
  in

  dist < cur_dist

let not_found pt state =
  let mask = Hashtbl.find state.grid.keys pt |> key_mask in

  state.have_keys land mask = 0

let find_to_visit node state =
  let candidates = Hashtbl.find state.dists node.pt in

  Hashtbl.iter
    (fun pt kid ->
      let from_3d = { i = node.pt.x; j = node.pt.y; k = node.keys } in
      let from_dist =
        try Hashtbl.find state.grid_3d from_3d with Not_found -> 0
      in
      let pt_3d = { i = pt.x; j = pt.y; k = state.have_keys } in
      let new_dist = from_dist + kid.dist in

      if
        not_found pt state && keys_match kid state
        && is_closer pt_3d new_dist state
      then (
        Printf.printf "%d,%d,%x(%d) => %d,%d,%x %d\n" node.pt.x node.pt.y
          state.have_keys from_dist pt_3d.i pt_3d.j pt_3d.k new_dist;

        Hashtbl.replace state.grid_3d pt_3d new_dist;
        Hashtbl.replace state.unvisited pt kid))
    candidates;

  state

let visit node state =
  if node.keys = 0x1f then
    Printf.printf "FOUND ALL %d\n"
    @@ Hashtbl.find state.grid_3d
         { i = node.pt.x; j = node.pt.y; k = node.keys };

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

  let closest = find_closest state in
  let state = visit closest state in

  let closest = find_closest state in
  let state = visit closest state in

  let closest = find_closest state in
  let state = visit closest state in

  Printf.printf "GRAPH 3D\n";
  Hashtbl.iter
    (fun pt dist -> Printf.printf "  %d,%d,%x %d\n" pt.i pt.j pt.k dist)
    state.grid_3d;

  Printf.printf "closest %d,%d\n" closest.pt.x closest.pt.y;
  Hashtbl.iter
    (fun _ v -> Printf.printf "  %d,%d\n" v.pt.x v.pt.y)
    state.unvisited

let run file_name =
  let pieces = Parser.parse_input file_name in
  let dists = DistMap.build_map pieces in

  walk_map pieces.enter pieces dists;

  0
