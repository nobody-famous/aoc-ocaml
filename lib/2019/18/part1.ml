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

type node = { pt : point; dist : int }

type found_key = { pt : point; key : char; dist : int }

type next_nodes = { empty : node list; keys : found_key list }

let add_to_neighbors seen pt dist (data : pieces) neighbors =
  if Hashtbl.mem seen pt then neighbors
  else if Hashtbl.mem data.empty pt then
    { neighbors with empty = { pt; dist = dist + 1 } :: neighbors.empty }
  else if Hashtbl.mem data.keys pt then
    let key = Hashtbl.find data.keys pt in
    { neighbors with keys = { pt; key; dist = dist + 1 } :: neighbors.keys }
  else neighbors

let get_neighbors seen pt dist data =
  { empty = []; keys = [] }
  |> add_to_neighbors seen { pt with y = pt.y - 1 } dist data
  |> add_to_neighbors seen { pt with y = pt.y + 1 } dist data
  |> add_to_neighbors seen { pt with x = pt.x + 1 } dist data
  |> add_to_neighbors seen { pt with x = pt.x - 1 } dist data

let visit_nodes seen dist nodes data =
  List.iter (fun (n : node) -> Hashtbl.replace seen n.pt dist) nodes.empty;
  List.iter (fun n -> Hashtbl.replace seen n.pt dist) nodes.keys;
  List.fold_left
    (fun acc (n : node) ->
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

let rec walk_map pt (data : pieces) =
  if Hashtbl.length data.keys = 0 then Printf.printf "NO KEYS\n";
  let keys = find_keys pt data in

  (* Printf.printf "walk_map %c %d,%d [" from_key pt.x pt.y;
     List.iter (fun v -> Printf.printf " %c" v.key) keys;
     Printf.printf "] [";
     Hashtbl.iter (fun d _ -> Printf.printf " %c" d) data.doors;
     Printf.printf "]\n"; *)
  List.iter
    (fun found_key ->
      let empty_copy = Hashtbl.copy data.empty in
      let keys_copy = Hashtbl.copy data.keys in
      let doors_copy = Hashtbl.copy data.doors in

      Hashtbl.replace empty_copy found_key.pt '.';
      if Hashtbl.mem doors_copy found_key.key then
        Hashtbl.replace empty_copy (Hashtbl.find doors_copy found_key.key) '.';
      Hashtbl.remove keys_copy found_key.pt;
      Hashtbl.remove doors_copy found_key.key;

      (* Printf.printf "  %d,%d %c\n" found_key.pt.x found_key.pt.y found_key.key;

         Hashtbl.iter (fun _ v -> Printf.printf " %c" v) keys_copy;
         Printf.printf "\n"; *)
      walk_map found_key.pt
        { data with empty = empty_copy; keys = keys_copy; doors = doors_copy })
    keys;
  ()

let run file_name =
  let piece_data = Parser.parse_input file_name in

  (match piece_data.enter with
  | None -> failwith "No entrance"
  | Some pt -> walk_map pt piece_data);

  (* List.iter (fun k -> Printf.printf "%d,%d %d\n" k.pt.x k.pt.y k.dist) keys; *)
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
