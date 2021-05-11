open Utils

type turn = RIGHT | LEFT

let turn_to_string = function RIGHT -> "RIGHT" | LEFT -> "LEFT"

let run_machine mach =
  let rec loop m =
    let m = Intcode.step m in

    match Intcode.get_state m with
    | HALT -> m
    | RUN -> loop m
    | OUTPUT -> handle_output m |> loop
    | s ->
        let str = Intcode.state_to_string s in
        raise @@ Failure (Printf.sprintf "Unhandled state %s" str)
  in

  loop mach

let has_point board pt =
  try
    let _ = Hashtbl.find board pt in
    true
  with Not_found -> false

let get_turn state =
  let board = state.board in
  let robot_pt = state.robot in
  let robot_piece = Hashtbl.find board robot_pt in

  match robot_piece with
  | ROBOT_UP ->
      if has_point board { robot_pt with col = robot_pt.col - 1 } then Some LEFT
      else if has_point board { robot_pt with col = robot_pt.col + 1 } then
        Some RIGHT
      else None
  | ROBOT_DOWN ->
      if has_point board { robot_pt with col = robot_pt.col + 1 } then Some LEFT
      else if has_point board { robot_pt with col = robot_pt.col - 1 } then
        Some RIGHT
      else None
  | ROBOT_LEFT ->
      if has_point board { robot_pt with row = robot_pt.row + 1 } then Some LEFT
      else if has_point board { robot_pt with row = robot_pt.row - 1 } then
        Some RIGHT
      else None
  | ROBOT_RIGHT ->
      if has_point board { robot_pt with row = robot_pt.row - 1 } then Some LEFT
      else if has_point board { robot_pt with row = robot_pt.row + 1 } then
        Some RIGHT
      else None
  | p -> raise @@ Failure (Printf.sprintf "get_turn %s" @@ piece_to_string p)

let turn_robot robot dir =
  match robot with
  | ROBOT_UP -> if dir = LEFT then ROBOT_LEFT else ROBOT_RIGHT
  | ROBOT_DOWN -> if dir = LEFT then ROBOT_RIGHT else ROBOT_LEFT
  | ROBOT_LEFT -> if dir = LEFT then ROBOT_DOWN else ROBOT_UP
  | ROBOT_RIGHT -> if dir = LEFT then ROBOT_UP else ROBOT_DOWN
  | _ -> robot

let forward state =
  let robot = Hashtbl.find state.board state.robot in
  let diff =
    match robot with
    | ROBOT_UP -> { row = -1; col = 0 }
    | ROBOT_DOWN -> { row = 1; col = 0 }
    | ROBOT_RIGHT -> { row = 0; col = 1 }
    | ROBOT_LEFT -> { row = 0; col = -1 }
    | _ -> { row = 0; col = 0 }
  in

  let rec loop pt count =
    let next_pt = { row = pt.row + diff.row; col = pt.col + diff.col } in
    let next_item =
      try Hashtbl.find state.board next_pt with Not_found -> SPACE
    in

    match next_item with
    | SCAFFOLD ->
        Hashtbl.replace state.board next_pt robot;
        Hashtbl.replace state.board pt SCAFFOLD;

        loop next_pt (count + 1)
    | _ -> ({ state with robot = pt }, count)
  in

  loop state.robot 0

type path_elem = { dir : turn; dist : int }

let walk_path in_state =
  let rec loop state path =
    let turn = get_turn state in

    match turn with
    | None -> List.rev path
    | Some t ->
        let robot = Hashtbl.find state.board state.robot in
        Hashtbl.replace state.board state.robot @@ turn_robot robot t;
        let state, count = forward state in

        loop state ({ dir = t; dist = count } :: path)
  in

  loop in_state []

let gen_path_opts path =
  let path_opts = Array.make (List.length path) [] in
  let add_to_opts elem ndx =
    let dir = if elem.dir = RIGHT then 'R' else 'L' in

    let rec loop n =
      if n <= ndx then (
        let opts = path_opts.(n) in
        let new_opt =
          if List.length opts = 0 then Printf.sprintf "%c,%d" dir elem.dist
          else Printf.sprintf "%s,%c,%d" (List.nth opts 0) dir elem.dist
        in

        if String.length new_opt < 20 then
          path_opts.(n) <- new_opt :: path_opts.(n);

        loop (n + 1))
    in

    loop 0
  in

  let rec loop p ndx =
    match p with
    | [] -> ()
    | first :: rest ->
        add_to_opts first ndx;
        loop rest (ndx + 1)
  in

  loop path 0;
  path_opts

type path_state = {
  a_fn : string option;
  b_fn : string option;
  c_fn : string option;
  fn_path : string;
  ndx : int;
}

let new_path_state () =
  { a_fn = None; b_fn = None; c_fn = None; fn_path = ""; ndx = 0 }

let append_str base str =
  if base = "" then str else Printf.sprintf "%s,%s" base str

let update_a_fn fn state =
  match state.a_fn with
  | None -> { state with a_fn = Some fn }
  | Some f ->
      if f = fn then { state with fn_path = append_str state.fn_path "A" }
      else state

let update_b_fn fn state =
  match state.b_fn with
  | None -> { state with b_fn = Some fn }
  | Some f ->
      if f = fn then { state with fn_path = append_str state.fn_path "B" }
      else state

let update_c_fn fn state =
  match state.c_fn with
  | None -> { state with c_fn = Some fn }
  | Some f ->
      if f = fn then { state with fn_path = append_str state.fn_path "C" }
      else state

let update_fns fn state =
  let new_state = update_a_fn fn state in
  let new_state =
    if state == new_state then update_b_fn fn state else new_state
  in
  let new_state =
    if state == new_state then update_c_fn fn state else new_state
  in

  new_state

let traverse_opts (path_opts : string list array) =
  let rec traverse state =
    if state.ndx >= Array.length path_opts then state
    else
      let opts = path_opts.(state.ndx) in

      match opts with
      | first :: _ ->
          let state = update_fns first state in
          traverse { state with ndx = state.ndx + List.length opts }
      | [] -> state
  in

  traverse (new_path_state ())

let run file_name =
  let state =
    Intcode.parse_input file_name
    |> Intcode.new_machine new_state
    |> run_machine |> Intcode.get_payload
  in

  let path = walk_path state in
  let path_opts = gen_path_opts path in

  let state = traverse_opts path_opts in

  (match state.a_fn with
  | None -> Printf.printf "No A Function\n"
  | Some fn -> Printf.printf "A: %s\n" fn);

  (match state.b_fn with
  | None -> Printf.printf "No B Function\n"
  | Some fn -> Printf.printf "B: %s\n" fn);

  (match state.c_fn with
  | None -> Printf.printf "No C Function\n"
  | Some fn -> Printf.printf "C: %s\n" fn);

  Array.iter
    (fun elem ->
      List.iter (fun s -> Printf.printf "[%s] " s) elem;
      Printf.printf "\n")
    path_opts;

  0
