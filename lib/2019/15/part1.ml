type move = NORTH | SOUTH | EAST | WEST

type status = HIT_WALL | MOVED | FOUND_SYS

type piece = WALL | EMPTY | OXYGEN_SYS

type point = { x : int; y : int }

type robot_state = {
  mach : Intcode.machine;
  loc : point;
  board : (point, piece) Hashtbl.t;
}

let new_state prog =
  let board = Hashtbl.create 64 in
  let start_point = { x = 0; y = 0 } in

  Hashtbl.replace board start_point EMPTY;
  { mach = Intcode.new_machine prog; loc = start_point; board }

let move_to_int m =
  match m with NORTH -> 1 | SOUTH -> 2 | WEST -> 3 | EAST -> 4

let move_to_string m =
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

let next_move state =
  let north = { x = state.loc.x; y = state.loc.y - 1 }
  and south = { x = state.loc.x; y = state.loc.y + 1 }
  and east = { x = state.loc.x + 1; y = state.loc.y }
  and west = { x = state.loc.x - 1; y = state.loc.y } in

  let dir =
    if not @@ Hashtbl.mem state.board north then NORTH
    else if not @@ Hashtbl.mem state.board south then SOUTH
    else if not @@ Hashtbl.mem state.board east then EAST
    else if not @@ Hashtbl.mem state.board west then WEST
    else raise @@ Failure (Printf.sprintf "No available direction")
  in

  dir

let handle_input state mach =
  let move = state |> next_move |> move_to_int in

  Printf.printf "handle_input %d\n" move;

  Intcode.set_input mach move

let handle_output _ mach =
  Printf.printf "handle_output\n";
  let mach, out = Intcode.get_output mach in

  (match out with
  | None -> raise @@ Failure "Expected output, got none"
  | Some v ->
      let code = status_of_int v in
      Printf.printf "OUT %s\n" @@ status_to_string code);

  mach

let run file_name =
  let prog = Intcode.parse_input file_name in
  let state = ref @@ new_state prog in

  let robot_input mach =
    state := { !state with mach = handle_input !state mach };
    !state.mach
  in

  let robot_output mach =
    state := { !state with mach = handle_output state mach };
    !state.mach
  in

  let _ =
    Intcode.new_machine prog |> Intcode.run_machine robot_input robot_output
  in

  0
