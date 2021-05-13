open AocUtils
open Utils

let find_entrance map =
  Hashtbl.fold
    (fun k (v : piece) acc -> if v = ENTRANCE then Some k else acc)
    map None

let add_to_neighbors seen pt map neighbors =
  if Hashtbl.mem seen pt then neighbors
  else
    let piece = try Hashtbl.find map pt with Not_found -> WALL in
    if piece <> WALL && piece <> ENTRANCE then pt :: neighbors else neighbors

let get_neighbors seen pt map =
  []
  |> add_to_neighbors seen { pt with y = pt.y - 1 } map
  |> add_to_neighbors seen { pt with y = pt.y + 1 } map
  |> add_to_neighbors seen { pt with x = pt.x + 1 } map
  |> add_to_neighbors seen { pt with x = pt.x - 1 } map

let find_keys start map =
  let seen = Hashtbl.create 64 in
  let neighbors = get_neighbors seen start map in

  Printf.printf "%d\n" @@ List.length neighbors;
  ()

let run file_name =
  let piece_map = Parser.parse_input file_name in
  let bounds = get_board_bounds piece_map in
  let enter = find_entrance piece_map in

  Printf.printf "%d,%d to %d,%d\n" bounds.low_x bounds.low_y bounds.high_x
    bounds.high_y;

  (match enter with
  | None -> Printf.printf "No entrance"
  | Some pt -> find_keys pt piece_map);

  0
