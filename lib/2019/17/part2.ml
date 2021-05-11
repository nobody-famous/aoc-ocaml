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
  str_path : string list;
  is_done : bool;
}

let new_path_state () =
  {
    a_fn = None;
    b_fn = None;
    c_fn = None;
    fn_path = "";
    str_path = [];
    is_done = false;
  }

let state_to_string state =
  let a_str = match state.a_fn with None -> "None" | Some f -> f in
  let b_str = match state.b_fn with None -> "None" | Some f -> f in
  let c_str = match state.c_fn with None -> "None" | Some f -> f in

  Printf.sprintf "{A: %s B: %s C: %s fns: %s}" a_str b_str c_str state.fn_path

let append_str base str =
  if base = "" then str else Printf.sprintf "%s,%s" base str

let update_a_fn fn state =
  match state.a_fn with
  | None ->
      { state with a_fn = Some fn; fn_path = append_str state.fn_path "A" }
  | Some f ->
      if f = fn then { state with fn_path = append_str state.fn_path "A" }
      else state

let update_b_fn fn state =
  match state.b_fn with
  | None ->
      { state with b_fn = Some fn; fn_path = append_str state.fn_path "B" }
  | Some f ->
      if f = fn then { state with fn_path = append_str state.fn_path "B" }
      else state

let update_c_fn fn state =
  match state.c_fn with
  | None ->
      { state with c_fn = Some fn; fn_path = append_str state.fn_path "C" }
  | Some f ->
      if f = fn then { state with fn_path = append_str state.fn_path "C" }
      else state

let update_fn fn opt orig_state state =
  if orig_state == state then fn opt state else state

let update_fns opt state =
  update_fn update_a_fn opt state state
  |> update_fn update_b_fn opt state
  |> update_fn update_c_fn opt state

let list_to_string arr =
  List.fold_left (fun s n -> Printf.sprintf "%s %s" s n) "" arr

(* let traverse_opts path_opts =
  let rec traverse state =
    if state.ndx >= Array.length path_opts then state
    else
      let opts = path_opts.(state.ndx) in

      Printf.printf "traverse %d %s\n" (Array.length path_opts)
        (state_to_string state);

      let rec try_opts opts' state =
        Printf.printf "try_opts %s\n" (list_to_string opts');
        match opts' with
        | first :: rest ->
            let new_state = update_fns first state in
            let new_state =
              traverse
                { new_state with ndx = new_state.ndx + List.length opts' }
            in

            if new_state == state then try_opts rest new_state else new_state
        | [] -> state
      in

      let state = try_opts opts state in
      Printf.printf "%s\n" (state_to_string state);
      state
  in

  traverse (new_path_state ()) *)

let traverse_opts path_opts =
  let rec loop ndx state =
    let opts = path_opts.(ndx) in

    let rec opts_loop opts state' =
      match opts with
      | [] -> state'
      | first :: rest ->
          let ndx' = ndx + List.length opts in
          let state' = update_fns first state in

          let state' =
            if state' = state then state
            else if ndx' > Array.length path_opts then state
            else if ndx' = Array.length path_opts then
              {
                state' with
                str_path = first :: state'.str_path;
                is_done = true;
              }
            else
              loop
                (ndx + List.length opts)
                { state' with str_path = first :: state'.str_path }
          in

          if state'.is_done then state' else opts_loop rest state'
    in

    opts_loop opts state
  in

  let state = loop 0 @@ new_path_state () in

  { state with str_path = List.rev state.str_path }

let str_to_input str =
  let buffer = Array.make (String.length str) @@ Char.code '\n' in

  let loop ndx =
    if ndx < String.length str then
      buffer.(ndx) <- Char.code @@ String.get str ndx
  in

  loop 0;
  buffer

let send_input input mach =
  let rec loop ndx m =
    if ndx >= Array.length input then m
    else
      let m = Intcode.step m in

      match Intcode.get_state m with
      | RUN -> loop ndx m
      | HALT -> m
      | INPUT ->
          Printf.printf "INPUT\n";
          m
      | OUTPUT ->
          let _, out = Intcode.get_output m in

          (match out with
          | None -> ()
          | Some _ -> ());
          loop ndx m
  in

  let prog' = Intcode.get_prog mach in
  Printf.printf "ADDR 0 %d\n" prog'.(0);

  Array.iter (fun n -> Printf.printf " %d" n) prog';
  Printf.printf "\n";
  loop 0 mach

let run file_name =
  let prog = Intcode.parse_input file_name in
  let state =
    Intcode.new_machine new_state (Array.copy prog)
    |> run_machine |> Intcode.get_payload |> walk_path |> gen_path_opts
    |> traverse_opts
  in

  Printf.printf "%s\n" @@ state_to_string state;

  let a_fn =
    match state.a_fn with
    | None -> raise @@ Failure "Fn A not defined"
    | Some f -> f
  in

  let b_fn =
    match state.b_fn with
    | None -> raise @@ Failure "Fn B not defined"
    | Some f -> f
  in

  let c_fn =
    match state.c_fn with
    | None -> raise @@ Failure "Fn C not defined"
    | Some f -> f
  in

  let fns_str = Printf.sprintf "%s\n%s\n%s\n" a_fn b_fn c_fn in
  let fns_buf = str_to_input fns_str in
  let path_str = Printf.sprintf "%s\n" state.fn_path in
  let path_buf = str_to_input path_str in

  Printf.printf "%d\n" @@ Array.length fns_buf;
  Printf.printf "%d\n" @@ Array.length path_buf;

  let mach = Intcode.new_machine () (Array.copy prog) in
  let mach = Intcode.set_addr mach 0 2 in

  let _ = send_input path_buf mach in

  0
