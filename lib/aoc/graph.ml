type position = { row : int; col : int }
type 'a edge = { target : position; weight : 'a }
type 'a node = { edges : 'a edge list }
type 'a graph = (position, 'a node) Hashtbl.t
type 'a visited_node = { path : position list; weight : 'a }

type 'a path_state = {
  visited : (position, 'a visited_node) Hashtbl.t;
  frontier : (position, int) Hashtbl.t;
}

let shortest_path ~start_pos:s ~end_pos:e ~init_weight:w graph =
  let update_frontier pos state = state in

  let create_state visited = { visited; frontier = Hashtbl.create 64 } in

  let init_state =
    [ (s, { path = [ s ]; weight = w }) ]
    |> List.to_seq
    |> Hashtbl.of_seq
    |> create_state
    |> update_frontier s
  in

  let find_path state = 0 in

  init_state |> find_path
