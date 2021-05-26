open AocUtils
open Utils

(* type node = { pt : point; dist : int }

type found_key = { pt : point; key : char; dist : int }

type next_nodes = { empty : node list; keys : found_key list }

let add_to_neighbors seen pt dist (data : pieces) neighbors =
  if Hashtbl.mem seen pt then neighbors
  else if Hashtbl.mem data.empty pt then
    { neighbors with empty = { pt; dist = dist + 1 } :: neighbors.empty }
  else if Hashtbl.mem data.keys pt then
    let key = Hashtbl.find data.keys pt in
    { neighbors with keys = { pt; key; dist = dist + 1 } :: neighbors.keys }
  else neighbors

let get_neighbors seen pt dist data =
  { empty = []; keys = [] }
  |> add_to_neighbors seen { pt with y = pt.y - 1 } dist data
  |> add_to_neighbors seen { pt with y = pt.y + 1 } dist data
  |> add_to_neighbors seen { pt with x = pt.x + 1 } dist data
  |> add_to_neighbors seen { pt with x = pt.x - 1 } dist data

let visit_nodes seen dist nodes data =
  List.iter (fun (n : node) -> Hashtbl.replace seen n.pt dist) nodes.empty;
  List.iter (fun n -> Hashtbl.replace seen n.pt dist) nodes.keys;
  List.fold_left
    (fun acc (n : node) ->
      let neighbors = get_neighbors seen n.pt dist data in
      { empty = acc.empty @ neighbors.empty; keys = acc.keys @ neighbors.keys })
    { empty = []; keys = nodes.keys }
    nodes.empty

let find_keys pt data =
  let seen = Hashtbl.create 64 in
  let nodes = get_neighbors seen pt 0 data in

  let rec loop dist nodes' =
    if List.length nodes'.empty = 0 then nodes'.keys
    else loop (dist + 1) @@ visit_nodes seen dist nodes' data
  in

  loop 1 nodes

let new_data found_key (data : pieces) =
  let empty_copy = Hashtbl.copy data.empty in
  let keys_copy = Hashtbl.copy data.keys in
  let doors_copy = Hashtbl.copy data.doors in

  Hashtbl.replace empty_copy found_key.pt '.';
  if Hashtbl.mem doors_copy found_key.key then
    Hashtbl.replace empty_copy (Hashtbl.find doors_copy found_key.key) '.';
  Hashtbl.remove keys_copy found_key.pt;
  Hashtbl.remove doors_copy found_key.key;

  (match data.enter with
  | None -> ()
  | Some pt -> Hashtbl.replace empty_copy pt '.');

  { enter = None; empty = empty_copy; keys = keys_copy; doors = doors_copy }

let rec walk_map pt min_steps dist (data : pieces) =
  if Hashtbl.length data.keys = 0 then Stdlib.min min_steps dist
  else
    let keys = find_keys pt data in

    let rec loop steps found_keys =
      match found_keys with
      | [] -> steps
      | key :: rest ->
          let steps' =
            new_data key data |> walk_map key.pt steps (dist + key.dist)
          in

          loop steps' rest
    in

    loop min_steps keys *)

type key_dist = { dist : int; keys : int }

type graph_node = { pt : point; keys : int }

type graph_state = {
  seen : (point, bool) Hashtbl.t;
  grid : pieces;
  dist : int;
  dist_map : (char, key_dist) Hashtbl.t;
}

let new_graph_state data =
  {
    seen = Hashtbl.create 64;
    grid = data;
    dist = 0;
    dist_map = Hashtbl.create 64;
  }

let mark_seen pt state =
  Hashtbl.replace state.seen pt true;
  state

let visit_empty pt state = { state with dist = state.dist + 1 }

let visit_key pt state = { state with dist = state.dist + 1 }

let visit_node pt state =
  if Hashtbl.mem state.seen pt then state
  else if Hashtbl.mem state.grid.empty pt then visit_empty pt state
  else if Hashtbl.mem state.grid.keys pt then visit_key pt state
  else if Hashtbl.mem state.grid.doors pt then state
  else state

let all_neighbors pt data =
  let state = new_graph_state data in

  let pt' = { pt with y = pt.y + 1 } in
  state |> visit_node pt' |> mark_seen pt'

let run file_name =
  let piece_data = Parser.parse_input file_name in

  (* let steps =
       match piece_data.enter with
       | None -> failwith "No entrance"
       | Some pt -> walk_map pt Int.max_int 0 piece_data
     in

     Printf.printf "steps %d\n" steps; *)
  0
