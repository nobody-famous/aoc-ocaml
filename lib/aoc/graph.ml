type position = { row : int; col : int }
type ('pos, 'w) edge = { target : 'pos; weight : 'w }
type ('pos, 'w) node = { edges : ('pos, 'w) edge list }
type ('pos, 'w) graph = ('pos, ('pos, 'w) node) Hashtbl.t
type ('pos, 'w) path_node = { path : 'pos list; weight : 'w }

type ('pos, 'w) path_state = {
  visited : ('pos, ('pos, 'w) path_node) Hashtbl.t;
  frontier : ('pos, ('pos, 'w) path_node) Hashtbl.t;
}

let shortest_path ~start_pos:s ~end_pos:e ~init_weight:w graph =
  let add_to_frontier state pos path weight =
    match Hashtbl.find_opt state.frontier pos with
    | Some n when weight < n.weight ->
        Hashtbl.replace state.frontier pos { path; weight }
    | Some _ -> ()
    | None -> Hashtbl.add state.frontier pos { path; weight }
  in

  let update_frontier state node =
    node.path
    |> List.hd
    |> Hashtbl.find graph
    |> (fun n -> n.edges)
    |> List.iter (fun e ->
           add_to_frontier state e.target (e.target :: node.path) e.weight);

    state
  in

  let create_state visited = { visited; frontier = Hashtbl.create 64 } in

  let init_state =
    [ (s, { path = [ s ]; weight = w }) ]
    |> List.to_seq
    |> Hashtbl.of_seq
    |> create_state
  in

  let init_frontier state =
    s |> Hashtbl.find state.visited |> update_frontier state
  in

  let find_path state =
    Hashtbl.iter
      (fun key value ->
        Printf.printf "%d %d [" key value.weight;
        List.iter (fun p -> Printf.printf "%d " p) value.path;
        Printf.printf "]\n")
      state.frontier;
    { path = []; weight = w }
  in

  init_state |> init_frontier |> find_path
