type direction = NORTH | SOUTH | EAST | WEST

type status = HIT_WALL | MOVED | FOUND_SYS

type piece = WALL | EMPTY | OXYGEN_SYS

type point = { x : int; y : int }

type robot_move = { dir : direction; is_bt : bool }

type robot_state = {
  sys_loc : point option;
  loc : point;
  to_check : point;
  move : robot_move;
  bt_list : direction list;
  board : (point, piece) Hashtbl.t;
}

let halt_machine mach = Intcode.set_state mach Intcode.HALT

let new_state () =
  let board = Hashtbl.create 64 in
  let start_point = { x = 0; y = 0 } in

  Hashtbl.replace board start_point EMPTY;
  {
    sys_loc = None;
    loc = start_point;
    to_check = start_point;
    move = { dir = NORTH; is_bt = false };
    bt_list = [];
    board;
  }

let piece_to_string p =
  match p with WALL -> "WALL" | EMPTY -> "EMPTY" | OXYGEN_SYS -> "OXYGEN_SYS"

let print_board state =
  Hashtbl.iter
    (fun k v -> Printf.printf "%d,%d %s\n" k.x k.y (piece_to_string v))
    state.board

let dir_to_int m =
  match m with NORTH -> 1 | SOUTH -> 2 | WEST -> 3 | EAST -> 4

let dir_to_opp dir =
  match dir with NORTH -> SOUTH | SOUTH -> NORTH | WEST -> EAST | EAST -> WEST

let dir_to_string m =
  match m with
  | NORTH -> "NORTH"
  | SOUTH -> "SOUTH"
  | EAST -> "EAST"
  | WEST -> "WEST"

let status_of_int v =
  match v with
  | 0 -> HIT_WALL
  | 1 -> MOVED
  | 2 -> FOUND_SYS
  | s -> raise @@ Failure (Printf.sprintf "Invalid status code %d" s)

let status_to_string s =
  match s with
  | HIT_WALL -> "WALL"
  | MOVED -> "MOVED"
  | FOUND_SYS -> "OXYGEN_SYS"

let next_move dir =
  match dir with NORTH -> EAST | SOUTH -> WEST | EAST -> SOUTH | WEST -> NORTH

let loc_to_check state =
  let loc = state.loc in

  match state.move.dir with
  | NORTH -> { state with to_check = { loc with y = loc.y + 1 } }
  | SOUTH -> { state with to_check = { loc with y = loc.y - 1 } }
  | EAST -> { state with to_check = { loc with x = loc.x + 1 } }
  | WEST -> { state with to_check = { loc with x = loc.x - 1 } }

let untried_dir state =
  let loc = state.loc in
  let north = { loc with y = loc.y + 1 } in
  let south = { loc with y = loc.y - 1 } in
  let east = { loc with x = loc.x + 1 } in
  let west = { loc with x = loc.x - 1 } in

  if not @@ Hashtbl.mem state.board north then Some NORTH
  else if not @@ Hashtbl.mem state.board south then Some SOUTH
  else if not @@ Hashtbl.mem state.board east then Some EAST
  else if not @@ Hashtbl.mem state.board west then Some WEST
  else None

let handle_input mach =
  let state = Intcode.get_payload mach in
  let move = dir_to_int state.move.dir in
  let new_state = loc_to_check state in

  let mach = Intcode.set_payload mach new_state in
  Intcode.set_input mach move

let pop_list lst = match lst with [] -> [] | _ :: rest -> rest

let backtrack state =
  match state.bt_list with
  | top :: rest ->
      {
        state with
        move = { dir = dir_to_opp top; is_bt = true };
        bt_list = rest;
      }
  | [] -> raise @@ Failure (Printf.sprintf "Backtrack ran out")

let handle_hit_wall mach =
  let state = Intcode.get_payload mach in

  Hashtbl.replace state.board state.to_check WALL;

  let untried = untried_dir state in

  let state =
    match untried with
    | None -> backtrack state
    | Some dir -> { state with move = { state.move with dir } }
  in

  Intcode.set_payload mach state

let print_bt state =
  List.iter (fun bt -> Printf.printf "%s " (dir_to_string bt)) state.bt_list;
  Printf.printf "\n"

let handle_moved mach =
  let state = Intcode.get_payload mach in
  let state =
    if state.move.is_bt then state
    else { state with bt_list = state.move.dir :: state.bt_list }
  in
  let state = { state with loc = state.to_check } in

  let state =
    match untried_dir state with
    | None -> backtrack state
    | Some dir -> { state with move = { dir; is_bt = false } }
  in

  Hashtbl.replace state.board state.loc EMPTY;
  Intcode.set_payload mach state

let handle_out_code code mach =
  let state = Intcode.get_payload mach in

  match code with
  | HIT_WALL -> handle_hit_wall mach
  | MOVED -> handle_moved mach
  | FOUND_SYS ->
      let state = { state with sys_loc = Some state.loc } in
      let mach = Intcode.set_payload mach state in

      halt_machine mach

let handle_output mach =
  let mach, out = Intcode.get_output mach in

  match out with
  | None -> raise @@ Failure "Expected output, got none"
  | Some v ->
      let code = status_of_int v in
      handle_out_code code mach

let run file_name =
  let prog = Intcode.parse_input file_name in
  let state = new_state () in

  let mach =
    Intcode.new_machine state prog
    |> Intcode.run_machine handle_input handle_output
  in
  let state = Intcode.get_payload mach in

  match state.sys_loc with
  | None -> Printf.printf "Did not find it"
  | Some loc -> Printf.printf "Found it at %d,%d\n" loc.x loc.y
