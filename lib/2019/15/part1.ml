type direction = NORTH | SOUTH | EAST | WEST

type status = HIT_WALL | MOVED | FOUND_SYS

type piece = WALL | EMPTY | OXYGEN_SYS

type point = { x : int; y : int }

type robot_state = {
  loc : point;
  move : direction;
  board : (point, piece) Hashtbl.t;
}

let new_state () =
  let board = Hashtbl.create 64 in
  let start_point = { x = 0; y = 0 } in

  Hashtbl.replace board start_point EMPTY;
  { loc = start_point; move = NORTH; board }

let dir_to_int m =
  match m with NORTH -> 1 | SOUTH -> 2 | WEST -> 3 | EAST -> 4

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

let move_to_loc state =
  let loc = state.loc in

  Printf.printf "move_to_loc %s\n" (dir_to_string state.move);
  match state.move with
  | NORTH -> { state with loc = { loc with y = loc.y - 1 } }
  | SOUTH -> { state with loc = { loc with y = loc.y + 1 } }
  | EAST -> { state with loc = { loc with x = loc.x + 1 } }
  | WEST -> { state with loc = { loc with x = loc.x - 1 } }

let handle_input mach =
  let stack = Intcode.get_payload mach in
  let state, move =
    match stack with
    | first :: _ -> (first, dir_to_int first.move)
    | [] -> raise @@ Failure (Printf.sprintf "Input empty stack\n")
  in

  let new_state = move_to_loc state in
  let mach = Intcode.set_payload mach (new_state :: stack) in
  Intcode.set_input mach move

let handle_hit_wall mach =
  let state, rest =
    match Intcode.get_payload mach with
    | first :: rest -> (first, rest)
    | [] -> raise @@ Failure (Printf.sprintf "Wall no state")
  in
  let top, rest =
    match rest with
    | first :: rest -> (first, rest)
    | [] -> raise @@ Failure (Printf.sprintf "Hit WALL, no backtrack")
  in

  Printf.printf "Hit wall %d,%d\n" state.loc.x state.loc.y;
  Hashtbl.replace state.board state.loc WALL;
  Intcode.set_payload mach ({ top with move = next_move top.move } :: rest)

let handle_moved mach =
  let state, rest =
    match Intcode.get_payload mach with
    | first :: rest -> (first, rest)
    | [] -> raise @@ Failure (Printf.sprintf "Moved no state")
  in

  Printf.printf "Moved to %d,%d\n" state.loc.x state.loc.y;
  Hashtbl.replace state.board state.loc EMPTY;
  Intcode.set_payload mach ({ state with move = NORTH } :: rest)

let handle_out_code code mach =
  let state_list = Intcode.get_payload mach in
  let state, _ =
    match state_list with
    | first :: rest -> (first, rest)
    | [] -> raise @@ Failure (Printf.sprintf "Output no state")
  in

  Printf.printf "output state %d %d\n" state.loc.x state.loc.y;

  match code with
  | HIT_WALL -> handle_hit_wall mach
  | MOVED -> handle_moved mach
  | s ->
      Printf.printf "%d\n" (Hashtbl.length state.board);
      raise @@ Failure (Printf.sprintf "Unhandled code %s" (status_to_string s))

let handle_output mach =
  Printf.printf "handle_output\n";
  let mach, out = Intcode.get_output mach in

  match out with
  | None -> raise @@ Failure "Expected output, got none"
  | Some v ->
      let code = status_of_int v in
      handle_out_code code mach

let run file_name =
  let prog = Intcode.parse_input file_name in
  let state = new_state () in

  let _ =
    Intcode.new_machine [ state ] prog
    |> Intcode.run_machine handle_input handle_output
  in

  0
