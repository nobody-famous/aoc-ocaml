open AocUtils
open Utils

type key_dist = { dist : int; keys : int }

type graph_node = { pt : point; dist : int; keys : int }

type graph_state = {
  seen : (point, bool) Hashtbl.t;
  grid : pieces;
  dist_map : (point, key_dist) Hashtbl.t;
  to_visit : graph_node list;
}

let new_graph_state data =
  {
    seen = Hashtbl.create 64;
    grid = data;
    dist_map = Hashtbl.create 64;
    to_visit = [];
  }

let mark_seen pt state =
  Hashtbl.replace state.seen pt true;
  state

let visit_empty node state =
  let new_node = { node with dist = node.dist + 1 } in

  { state with to_visit = new_node :: state.to_visit }

let visit_key node state =
  let new_node = { node with dist = node.dist + 1 } in

  Hashtbl.replace state.dist_map node.pt
    { dist = node.dist + 1; keys = node.keys };

  { state with to_visit = new_node :: state.to_visit }

let visit_door node state =
  let mask = Hashtbl.find state.grid.doors node.pt |> door_to_key |> key_mask in
  let new_keys = node.keys lor mask in
  let new_node = { node with dist = node.dist + 1; keys = new_keys } in

  { state with to_visit = new_node :: state.to_visit }

let visit_node node state =
  if Hashtbl.mem state.seen node.pt then state
  else if Hashtbl.mem state.grid.empty node.pt then visit_empty node state
  else if Hashtbl.mem state.grid.keys node.pt then visit_key node state
  else if Hashtbl.mem state.grid.doors node.pt then visit_door node state
  else state

let visit node state =
  let node' = node in

  if Hashtbl.mem state.seen node'.pt then state
  else if Hashtbl.mem state.grid.empty node'.pt then visit_empty node' state
  else if Hashtbl.mem state.grid.keys node'.pt then visit_key node' state
  else if Hashtbl.mem state.grid.doors node'.pt then visit_door node' state
  else state

let node_neighbors node state =
  state |> mark_seen node.pt
  |> visit { node with pt = { node.pt with y = node.pt.y + 1 } }
  |> visit { node with pt = { node.pt with y = node.pt.y - 1 } }
  |> visit { node with pt = { node.pt with x = node.pt.x + 1 } }
  |> visit { node with pt = { node.pt with x = node.pt.x - 1 } }

let all_neighbors pt data =
  let rec loop state =
    match state.to_visit with
    | [] -> state.dist_map
    | nodes ->
        let next_nodes =
          List.fold_left
            (fun acc n ->
              let s = node_neighbors n { state with to_visit = [] } in
              s.to_visit @ acc)
            [] nodes
        in

        loop { state with to_visit = next_nodes }
  in

  let state = new_graph_state data in
  loop { state with to_visit = [ { pt; dist = 0; keys = 0 } ] }

let build_dist_map (data : pieces) =
  let keys = List.of_seq @@ Hashtbl.to_seq_keys data.keys in
  let roots = data.enter :: keys in
  let dists = Hashtbl.create 64 in

  List.iter
    (fun root -> Hashtbl.replace dists root @@ all_neighbors root data)
    roots;

  dists

let run file_name =
  let dists = Parser.parse_input file_name |> build_dist_map in

  Hashtbl.iter
    (fun pt kdists ->
      Printf.printf "DISTS %d,%d\n" pt.x pt.y;
      Hashtbl.iter
        (fun pt' (kdist : key_dist) ->
          Printf.printf "  %d,%d %d\n" pt'.x pt'.y kdist.dist)
        kdists)
    dists;

  0
