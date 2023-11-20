type ('pos, 'w) edge = { target : 'pos; weight : 'w }
type ('pos, 'w) node = { edges : ('pos, 'w) edge list }
type ('pos, 'w) graph = ('pos, ('pos, 'w) node) Hashtbl.t
type ('pos, 'w) path_node = { pos : 'pos; path : 'pos list; weight : 'w }

type ('pos, 'w) path_state = {
  visited : ('pos, bool) Hashtbl.t;
  frontier : ('pos, ('pos, 'w) path_node) Hashtbl.t;
}

let create_state visited = { visited; frontier = Hashtbl.create 64 }

let init_state start_pos =
  [ (start_pos, true) ] |> List.to_seq |> Hashtbl.of_seq |> create_state

let add_to_frontier state pos path weight =
  match Hashtbl.find_opt state.frontier pos with
  | Some n when weight < n.weight ->
      Hashtbl.replace state.frontier pos { pos; path; weight }
  | Some _ -> ()
  | None -> Hashtbl.add state.frontier pos { pos; path; weight }

let get_edges graph pos = pos |> Hashtbl.find graph |> fun n -> n.edges

let add_edges state node =
  List.iter (fun edge ->
      if Option.is_none @@ Hashtbl.find_opt state.visited edge.target then
        add_to_frontier state edge.target (edge.target :: node.path)
          (edge.weight + node.weight))

let init_frontier start_pos graph state =
  get_edges graph start_pos
  |> add_edges state { pos = start_pos; path = [ start_pos ]; weight = 0 };
  state

let next_node start_pos init_weight state =
  Hashtbl.fold
    (fun _ value node ->
      if node.path = [] then value
      else if Option.is_some (Hashtbl.find_opt state.visited node.pos) then node
      else if value.weight < node.weight then value
      else node)
    state.frontier
    { pos = start_pos; path = []; weight = init_weight }

let visit_node state node graph =
  node.pos |> get_edges graph |> add_edges state node;
  node.pos |> Hashtbl.remove state.frontier;

  Hashtbl.replace state.visited node.pos true;

  state

let rec find_path start_pos end_pos init_weight graph state =
  match next_node start_pos init_weight state with
  | n when n.pos = end_pos -> n
  | n ->
      visit_node state n graph |> find_path start_pos end_pos init_weight graph

let shortest_path ~start_pos:s ~end_pos:e ~init_weight:w graph =
  init_state s |> init_frontier s graph |> find_path s e w graph
