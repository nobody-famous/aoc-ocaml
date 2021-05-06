type move = NORTH | SOUTH | EAST | WEST

type status = HIT_WALL | MOVED | FOUND_SYS

type piece = WALL | EMPTY | OXYGEN_SYS

type point = { x : int; y : int }

type robot_state = {
  loc : point;
  to_move : move;
  board : (point, piece) Hashtbl.t;
}

let new_state () =
  let board = Hashtbl.create 64 in
  let start_point = { x = 0; y = 0 } in

  Hashtbl.replace board start_point EMPTY;
  { loc = start_point; to_move = NORTH; board }

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

let next_move dir =
  match dir with NORTH -> EAST | SOUTH -> WEST | EAST -> SOUTH | WEST -> NORTH

let move_to_loc state =
  let loc = state.loc in

  match state.to_move with
  | NORTH -> { state with loc = { loc with y = loc.y - 1 } }
  | SOUTH -> { state with loc = { loc with y = loc.y + 1 } }
  | EAST -> { state with loc = { loc with x = loc.x + 1 } }
  | WEST -> { state with loc = { loc with x = loc.x - 1 } }

let handle_input mach =
  let stack = Intcode.get_payload mach in
  let move =
    match stack with
    | first :: _ -> move_to_int first.to_move
    | [] -> raise @@ Failure (Printf.sprintf "Input empty stack\n")
  in

  Intcode.set_input mach move

let handle_output mach =
  Printf.printf "handle_output\n";
  let mach, out = Intcode.get_output mach in

  (match out with
  | None -> raise @@ Failure "Expected output, got none"
  | Some v ->
      let code = status_of_int v in
      Printf.printf "OUT %s\n" @@ status_to_string code);

  let mach = Intcode.set_state mach Intcode.HALT in
  mach

let run file_name =
  let prog = Intcode.parse_input file_name in
  let state = new_state () in

  let _ =
    Intcode.new_machine [ state ] prog
    |> Intcode.run_machine handle_input handle_output
  in

  0
