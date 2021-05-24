open AocUtils
open Utils

(* type node = { pt : point; piece : piece; dist : int }

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

  next.keys *)

type node = { pt : point; piece : piece; dist : int }

type next_nodes = { empty : node list; keys : node list }

let add_to_neighbors seen pt dist data neighbors =
  if Hashtbl.mem seen pt then neighbors
  else
    let piece = try Hashtbl.find data.empty pt with Not_found -> WALL in
    if is_empty piece then
      {
        neighbors with
        empty = { pt; piece; dist = dist + 1 } :: neighbors.empty;
      }
    else if is_key piece then
      { neighbors with keys = { pt; piece; dist = dist + 1 } :: neighbors.keys }
    else neighbors

let get_neighbors seen pt dist data =
  { empty = []; keys = [] }
  |> add_to_neighbors seen { pt with y = pt.y - 1 } dist data
  |> add_to_neighbors seen { pt with y = pt.y + 1 } dist data
  |> add_to_neighbors seen { pt with x = pt.x + 1 } dist data
  |> add_to_neighbors seen { pt with x = pt.x - 1 } dist data

let visit_nodes seen dist nodes data =
  List.iter (fun n -> Hashtbl.replace seen n.pt dist) nodes.empty;
  List.fold_left
    (fun acc n ->
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

let run file_name =
  let piece_data = Parser.parse_input file_name in

  let keys =
    match piece_data.enter with
    | None -> failwith "No entrance"
    | Some pt -> find_keys pt piece_data
  in

  Printf.printf "Keys %d" @@ List.length keys;
  0

(* let keys =
     match piece_data.enter with
     | None -> []
     | Some pt -> find_keys pt piece_data.keys
   in

   Printf.printf "%d\n" @@ List.length keys;
   List.iter
     (fun k ->
       Printf.printf "%d,%d %d %c\n" k.pt.x k.pt.y k.dist
         (match k.piece with KEY v -> v | DOOR v -> v | _ -> '0'))
     keys;
   0 *)
