module G = Aoc.Graph

(* type position = { row : int; col : int } *)
type cell = { pos : G.position; ch : char }

(* type grid = { s : G.position; e : G.position; cells : cell list } *)
type grid = { s : G.position; e : G.position; graph : int G.graph }

(* type 'a edge = { weight : 'a }
   type 'a node = { pos : Aoc.Graph.position; edges : 'a edge list }

   type 'a graph = {
     startPos : position;
     endPos : position;
     nodes : (position, 'a node) Hashtbl.t;
   } *)

let to_cells row (line : string) =
  line
  |> String.to_seq
  |> List.of_seq
  |> List.mapi (fun col ch -> { pos = { row; col }; ch })

let get_cells data =
  let update (s, e, cells) item =
    match item.ch with
    | 'S' -> (item.pos, e, { pos = item.pos; ch = 'a' } :: cells)
    | 'E' -> (s, item.pos, { pos = item.pos; ch = 'z' } :: cells)
    | _ -> (s, e, item :: cells)
  in

  List.fold_left update
    ({ G.row = 0; G.col = 0 }, { G.row = 0; G.col = 0 }, [])
    data

let build_graph cells =
  let get_candidates (pos : G.position) =
    [
      { G.row = pos.row - 1; G.col = pos.col };
      { G.row = pos.row + 1; G.col = pos.col };
      { G.row = pos.row; G.col = pos.col - 1 };
      { G.row = pos.row; G.col = pos.col + 1 };
    ]
  in

  let to_edge p : 'a G.edge option =
    Some { G.target = { G.row = 0; G.col = 0 }; G.weight = 0 }
  in

  let create_node p es = { G.edges = es } in

  let insert_node nodes cell =
    get_candidates cell.pos
    |> List.map to_edge
    |> List.filter Option.is_some
    |> List.map Option.get
    |> create_node cell.pos
    |> Hashtbl.replace nodes cell.pos;
    nodes
  in

  List.fold_left insert_node (Hashtbl.create 64) cells

(* let toGraph grid =
   let graph =
     { startPos = grid.s; endPos = grid.e; nodes = cellsToNodes grid.cells }
   in

   graph *)

let build_grid data =
  let s, e, cells = get_cells data in
  { s; e; graph = build_graph cells }

let parse_input lines = List.mapi to_cells lines |> List.flatten |> build_grid
