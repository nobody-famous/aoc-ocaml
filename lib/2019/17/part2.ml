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

let check_west board pt =
  try
    let _ = Hashtbl.find board { pt with col = pt.col - 1 } in
    true
  with Not_found -> false

let check_east board pt =
  try
    let _ = Hashtbl.find board { pt with col = pt.col + 1 } in
    true
  with Not_found -> false

let get_turn state =
  let board = state.board in
  let robot_pt = state.robot in
  let robot_piece = Hashtbl.find board robot_pt in

  match robot_piece with
  | ROBOT_UP ->
      if check_west board robot_pt then Some LEFT
      else if check_east board robot_pt then Some RIGHT
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
    | ROBOT_UP -> { row = 1; col = 0 }
    | ROBOT_DOWN -> { row = -1; col = 0 }
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

let walk_path in_state =
  let rec loop state =
    let pt = state.robot in
    Printf.printf "loop %s\n" @@ point_to_string pt;

    let turn = get_turn state in
    match turn with
    | None -> raise @@ Failure "No turn"
    | Some t ->
        let robot = Hashtbl.find state.board state.robot in
        Hashtbl.replace state.board state.robot @@ turn_robot robot t;
        let state, count = forward state in

        Printf.printf "turn %s %d\n" (point_to_string state.robot) count;
        loop state
  in

  loop in_state

let run file_name =
  let state =
    Intcode.parse_input file_name
    |> Intcode.new_machine new_state
    |> run_machine |> Intcode.get_payload
  in

  if true then walk_path state;

  print_board state.board;

  0
