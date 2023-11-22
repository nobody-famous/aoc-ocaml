open Utils
open AocUtils

type turn = RIGHT | LEFT

let turn_to_string = function
  | RIGHT -> "RIGHT"
  | LEFT -> "LEFT"

let run_machine mach =
  let rec loop m =
    let m = Intcode.step m in

    match Intcode.get_state m with
    | Halt -> m
    | Run -> loop m
    | HasOutput -> handle_output m |> loop
    | NeedInput -> m
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
      if has_point board { robot_pt with y = robot_pt.y - 1 } then Some LEFT
      else if has_point board { robot_pt with y = robot_pt.y + 1 } then
        Some RIGHT
      else None
  | ROBOT_DOWN ->
      if has_point board { robot_pt with y = robot_pt.y + 1 } then Some LEFT
      else if has_point board { robot_pt with y = robot_pt.y - 1 } then
        Some RIGHT
      else None
  | ROBOT_LEFT ->
      if has_point board { robot_pt with x = robot_pt.x + 1 } then Some LEFT
      else if has_point board { robot_pt with x = robot_pt.x - 1 } then
        Some RIGHT
      else None
  | ROBOT_RIGHT ->
      if has_point board { robot_pt with x = robot_pt.x - 1 } then Some LEFT
      else if has_point board { robot_pt with x = robot_pt.x + 1 } then
        Some RIGHT
      else None
  | p -> failwith (Printf.sprintf "get_turn %s" @@ piece_to_string p)

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
    | ROBOT_UP -> { x = -1; y = 0 }
    | ROBOT_DOWN -> { x = 1; y = 0 }
    | ROBOT_RIGHT -> { x = 0; y = 1 }
    | ROBOT_LEFT -> { x = 0; y = -1 }
    | _ -> { x = 0; y = 0 }
  in

  let rec loop pt count =
    let next_pt = { x = pt.x + diff.x; y = pt.y + diff.y } in
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
  let a_str =
    match state.a_fn with
    | None -> "None"
    | Some f -> f
  in
  let b_str =
    match state.b_fn with
    | None -> "None"
    | Some f -> f
  in
  let c_str =
    match state.c_fn with
    | None -> "None"
    | Some f -> f
  in

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
  if orig_state = state then fn opt state else state

let update_fns opt state =
  update_fn update_a_fn opt state state
  |> update_fn update_b_fn opt state
  |> update_fn update_c_fn opt state

let list_to_string arr =
  List.fold_left (fun s n -> Printf.sprintf "%s %s" s n) "" arr

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

let str_to_buffer str =
  let buffer = Array.make (String.length str) @@ Char.code '\n' in

  let rec loop ndx =
    if ndx < String.length str then (
      buffer.(ndx) <- Char.code @@ String.get str ndx;
      loop (ndx + 1))
  in

  loop 0;
  buffer

let send_input input mach =
  let rec loop ndx m =
    if ndx >= Array.length input then m
    else
      let m = Intcode.step m in

      match Intcode.get_state m with
      | Run -> loop ndx m
      | Halt -> m
      | NeedInput -> loop (ndx + 1) @@ Intcode.set_input input.(ndx) m
      | HasOutput ->
          let m, _ = Intcode.get_output m in
          loop ndx m
  in

  loop 0 mach

let final_output mach =
  let rec loop m out =
    let m = Intcode.step m in
    match Intcode.get_state m with
    | Halt -> out
    | Run -> loop m out
    | HasOutput -> (
        let m', out' = Intcode.get_output m in
        match out' with
        | Some v -> loop m' v
        | _ -> failwith "No output")
    | s ->
        raise
        @@ Failure
             (Printf.sprintf "final_output %s" @@ Intcode.state_to_string s)
  in

  loop mach 0

let get_fns state =
  let a =
    match state.a_fn with
    | None -> failwith "Fn A not defined"
    | Some f -> f
  in
  let b =
    match state.b_fn with
    | None -> failwith "Fn B not defined"
    | Some f -> f
  in
  let c =
    match state.c_fn with
    | None -> failwith "Fn C not defined"
    | Some f -> f
  in

  (a, b, c)

let run lines =
  let mach =
    Intcode.parse_input lines
    |> Intcode.new_machine new_state
    |> Intcode.set_addr 0 2
    |> run_machine
  in
  let state =
    mach |> Intcode.get_payload |> walk_path |> gen_path_opts |> traverse_opts
  in

  let a_fn, b_fn, c_fn = get_fns state in

  let fns_buf = Printf.sprintf "%s\n%s\n%s\n" a_fn b_fn c_fn |> str_to_buffer in
  let path_buf = Printf.sprintf "%s\n" state.fn_path |> str_to_buffer in
  let yn_buf = Printf.sprintf "n\n" |> str_to_buffer in

  mach
  |> send_input path_buf
  |> send_input fns_buf
  |> send_input yn_buf
  |> final_output
