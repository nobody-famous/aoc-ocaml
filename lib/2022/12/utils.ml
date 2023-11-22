module G = Aoc.Graph

type position = { row : int; col : int }

type grid = {
  s : position;
  e : position;
  cells : (position, char) Hashtbl.t;
  graph : (position, int) G.graph;
}

let get_cells data =
  let update pos ch (s, e, cells) =
    match ch with
    | 'S' ->
        Hashtbl.replace cells pos 'a';
        (pos, e, cells)
    | 'E' ->
        Hashtbl.replace cells pos 'z';
        (s, pos, cells)
    | _ ->
        Hashtbl.replace cells pos ch;
        (s, e, cells)
  in

  Hashtbl.fold update data
    ({ row = 0; col = 0 }, { row = 0; col = 0 }, Hashtbl.create 64)

let create_node es = { G.edges = es }

let get_neighbors pos =
  [
    { row = pos.row - 1; col = pos.col };
    { row = pos.row + 1; col = pos.col };
    { row = pos.row; col = pos.col - 1 };
    { row = pos.row; col = pos.col + 1 };
  ]

let build_graph to_edge cells =
  let to_node pos ch nodes =
    get_neighbors pos
    |> List.map (fun n -> to_edge cells ch n)
    |> List.filter Option.is_some
    |> List.map Option.get
    |> create_node
    |> Hashtbl.replace nodes pos;

    nodes
  in

  Hashtbl.fold to_node cells (Hashtbl.create 64)

let build_grid to_edge data =
  let s, e, cells = get_cells data in
  { s; e; cells; graph = build_graph to_edge cells }
