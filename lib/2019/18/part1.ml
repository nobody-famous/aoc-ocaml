open AocUtils
open Utils

type node = { pt : point; piece : piece; dist : int }

type next_nodes = { empty : node list; keys : node list }

let find_entrance map =
  Hashtbl.fold
    (fun k (v : piece) acc -> if v = ENTRANCE then Some k else acc)
    map None

let add_to_neighbors seen pt dist map next =
  if Hashtbl.mem seen pt then next
  else
    let piece = try Hashtbl.find map pt with Not_found -> WALL in
    if is_empty piece then
      { next with empty = { pt; piece; dist = dist + 1 } :: next.empty }
    else if is_key piece || is_door piece then
      { next with keys = { pt; piece; dist = dist + 1 } :: next.keys }
    else next

let get_neighbors seen pt dist map =
  { empty = []; keys = [] }
  |> add_to_neighbors seen { pt with y = pt.y - 1 } dist map
  |> add_to_neighbors seen { pt with y = pt.y + 1 } dist map
  |> add_to_neighbors seen { pt with x = pt.x + 1 } dist map
  |> add_to_neighbors seen { pt with x = pt.x - 1 } dist map

let find_keys start map =
  let seen = Hashtbl.create 64 in
  let neighbors = get_neighbors seen start 0 map in

  let rec loop nodes dist =
    if List.length nodes.empty = 0 then nodes
    else (
      List.iter (fun n -> Hashtbl.replace seen n.pt dist) nodes.empty;

      let next =
        List.fold_left
          (fun acc n ->
            let next = get_neighbors seen n.pt dist map in
            { empty = acc.empty @ next.empty; keys = acc.keys @ next.keys })
          { empty = []; keys = nodes.keys }
          nodes.empty
      in

      loop next (dist + 1))
  in

  let next = loop neighbors 1 in

  next.keys

let run file_name =
  let piece_map = Parser.parse_input file_name in
  let bounds = get_board_bounds piece_map in
  let enter = find_entrance piece_map in

  Printf.printf "%d,%d to %d,%d\n" bounds.low_x bounds.low_y bounds.high_x
    bounds.high_y;

  let keys =
    match enter with None -> [] | Some pt -> find_keys pt piece_map
  in

  Printf.printf "%d\n" @@ List.length keys;
  List.iter
    (fun k ->
      Printf.printf "%d,%d %d %c\n" k.pt.x k.pt.y k.dist
        (match k.piece with KEY v -> v | DOOR v -> v | _ -> '0'))
    keys;
  0
