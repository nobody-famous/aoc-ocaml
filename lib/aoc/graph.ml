type position = { row : int; col : int }
type ('pos, 'w) edge = { target : 'pos; weight : 'w }
type ('pos, 'w) node = { edges : ('pos, 'w) edge list }
type ('pos, 'w) graph = ('pos, ('pos, 'w) node) Hashtbl.t
type ('pos, 'w) visited_node = { path : 'pos list; weight : 'w }

type ('pos, 'w) path_state = {
  visited : ('pos, ('pos, 'w) visited_node) Hashtbl.t;
  frontier : ('pos, int) Hashtbl.t;
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

  let find_path state = { path = []; weight = w } in

  init_state |> find_path
