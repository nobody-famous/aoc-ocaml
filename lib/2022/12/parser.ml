module G = Aoc.Graph

type cell = { pos : G.position; ch : char }

type grid = {
  s : G.position;
  e : G.position;
  graph : (G.position, int) G.graph;
}

let to_cells row (line : string) =
  line
  |> String.to_seq
  |> List.of_seq
  |> List.mapi (fun col ch -> { pos = { row; col }; ch })

let to_map =
  List.fold_left
    (fun ht c ->
      Hashtbl.replace ht c.pos c.ch;
      ht)
    (Hashtbl.create 64)

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
    ({ G.row = 0; G.col = 0 }, { G.row = 0; G.col = 0 }, Hashtbl.create 64)

let build_graph cells =
  let get_neighbors (pos : G.position) =
    [
      { G.row = pos.row - 1; G.col = pos.col };
      { G.row = pos.row + 1; G.col = pos.col };
      { G.row = pos.row; G.col = pos.col - 1 };
      { G.row = pos.row; G.col = pos.col + 1 };
    ]
  in

  let inc_char ch = ch |> Char.code |> ( + ) 1 |> Char.chr in

  let to_edge cells ch neighbor_pos =
    match Hashtbl.find_opt cells neighbor_pos with
    | Some neighbor_ch ->
        if neighbor_ch <= inc_char ch then
          Some { G.target = neighbor_pos; G.weight = 1 }
        else None
    | None -> None
  in

  let create_node es = { G.edges = es } in

  let to_node pos ch nodes =
    let node =
      get_neighbors pos
      |> List.map (fun n -> to_edge cells ch n)
      |> List.filter Option.is_some
      |> List.map Option.get
      |> create_node
    in

    Hashtbl.replace nodes pos node;

    nodes
  in

  let foo = Hashtbl.fold to_node cells (Hashtbl.create 64) in
  foo

let build_grid data =
  let s, e, cells = get_cells data in
  { s; e; graph = build_graph cells }

let parse_input lines =
  List.mapi to_cells lines |> List.flatten |> to_map |> build_grid
