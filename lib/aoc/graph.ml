(* type position = { row : int; col : int } *)
type ('pos, 'w) edge = { target : 'pos; weight : 'w }
type ('pos, 'w) node = { edges : ('pos, 'w) edge list }
type ('pos, 'w) graph = ('pos, ('pos, 'w) node) Hashtbl.t
type ('pos, 'w) path_node = { pos : 'pos; path : 'pos list; weight : 'w }

type ('pos, 'w) path_state = {
  visited : ('pos, bool) Hashtbl.t;
  frontier : ('pos, ('pos, 'w) path_node) Hashtbl.t;
}

let print_frontier state =
  Hashtbl.iter
    (fun k v -> Printf.printf "***** %d %d\n" k v.weight)
    state.frontier

let shortest_path ~start_pos:s ~end_pos:e ~init_weight:w graph =
  let add_to_frontier state pos path weight =
    match Hashtbl.find_opt state.frontier pos with
    | Some n when weight < n.weight ->
        Hashtbl.replace state.frontier pos { pos; path; weight }
    | Some _ -> ()
    | None -> Hashtbl.add state.frontier pos { pos; path; weight }
  in

  let get_edges pos = pos |> Hashtbl.find graph |> fun n -> n.edges in

  let add_edges state node =
    List.iter (fun e ->
        if Option.is_none @@ Hashtbl.find_opt state.visited e.target then
          add_to_frontier state e.target (e.target :: node.path)
            (e.weight + node.weight))
  in

  let create_state visited = { visited; frontier = Hashtbl.create 64 } in

  let init_state =
    [ (s, true) ] |> List.to_seq |> Hashtbl.of_seq |> create_state
  in

  let init_frontier state =
    get_edges s |> add_edges state { pos = s; path = [ s ]; weight = 0 };
    state
  in

  let next_node state =
    Hashtbl.fold
      (fun _ v n ->
        if n.path == [] then v
        else if Option.is_some (Hashtbl.find_opt state.visited n.pos) then n
        else if v.weight < n.weight then v
        else n)
      state.frontier
      { pos = s; path = []; weight = w }
  in

  let visit_node state node =
    node.pos |> get_edges |> add_edges state node;
    node.pos |> Hashtbl.remove state.frontier;

    Hashtbl.replace state.visited node.pos true;

    state
  in

  let find_path state =
    let _ = next_node state |> visit_node state in

    { pos = s; path = []; weight = w }
  in

  init_state |> init_frontier |> find_path
