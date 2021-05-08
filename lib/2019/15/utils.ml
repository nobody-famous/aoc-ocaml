type direction = NORTH | SOUTH | EAST | WEST

type status = HIT_WALL | MOVED | FOUND_SYS

type piece = WALL | EMPTY | OXYGEN_SYS | UNKNOWN

type point = { x : int; y : int }

type search_state = {
  loc : point;
  oxygen_sys : point option;
  path : direction list;
  visited : (point, piece) Hashtbl.t;
}

let new_state start_loc =
  let state =
    {
      loc = start_loc;
      oxygen_sys = None;
      path = [];
      visited = Hashtbl.create 64;
    }
  in

  Hashtbl.replace state.visited state.loc EMPTY;
  state

let status_of_int = function
  | 0 -> HIT_WALL
  | 1 -> MOVED
  | 2 -> FOUND_SYS
  | s -> raise @@ Failure (Printf.sprintf "Invalid status code %d" s)

let piece_to_string = function
  | WALL -> "WALL"
  | EMPTY -> "EMPTY"
  | OXYGEN_SYS -> "OXYGEN_SYS"
  | UNKNOWN -> "UNKNOWN"

let status_to_piece = function
  | HIT_WALL -> WALL
  | MOVED -> EMPTY
  | FOUND_SYS -> OXYGEN_SYS

let status_to_string = function
  | HIT_WALL -> "WALL"
  | MOVED -> "EMPTY"
  | FOUND_SYS -> "OXYGEN_SYS"

let dir_to_int dir =
  match dir with NORTH -> 1 | SOUTH -> 2 | WEST -> 3 | EAST -> 4

let dir_to_opp dir =
  match dir with NORTH -> SOUTH | SOUTH -> NORTH | WEST -> EAST | EAST -> WEST

let dir_to_string m =
  match m with
  | NORTH -> "NORTH"
  | SOUTH -> "SOUTH"
  | EAST -> "EAST"
  | WEST -> "WEST"

let next_move dir =
  match dir with NORTH -> EAST | SOUTH -> WEST | EAST -> SOUTH | WEST -> NORTH

let print_board board =
  let low_x, high_x, low_y, high_y =
    Hashtbl.fold
      (fun k _ (lx, hx, ly, hy) ->
        ( Stdlib.min lx k.x,
          Stdlib.max hx k.x,
          Stdlib.min ly k.y,
          Stdlib.max hy k.y ))
      board
      (max_int, min_int, max_int, min_int)
  in

  let rec y_loop y =
    let rec x_loop x =
      let piece = try Hashtbl.find board { x; y } with Not_found -> UNKNOWN in

      let ch =
        match piece with
        | EMPTY -> ' '
        | WALL -> '#'
        | OXYGEN_SYS -> 'X'
        | UNKNOWN -> '?'
      in

      let ch = if x = 0 && y = 0 then 'S' else ch in

      Printf.printf "%c" ch;
      if x <= high_x then x_loop (x + 1)
    in

    Printf.printf "?";
    x_loop low_x;
    Printf.printf "?\n";
    if y >= low_y then y_loop (y - 1)
  in

  y_loop high_y

let move_loc loc dir =
  match dir with
  | NORTH -> { loc with y = loc.y + 1 }
  | SOUTH -> { loc with y = loc.y - 1 }
  | EAST -> { loc with x = loc.x + 1 }
  | WEST -> { loc with x = loc.x - 1 }

let next_machine dir mach =
  let state = Intcode.get_payload mach in
  let new_loc = move_loc state.loc dir in

  mach
  |> Intcode.set_prog (Array.copy @@ Intcode.get_prog mach)
  |> Intcode.set_input (dir_to_int dir)
  |> Intcode.set_payload { state with loc = new_loc; path = dir :: state.path }

let next_direction mach dir mach_list =
  let state = Intcode.get_payload mach in
  let new_loc = move_loc state.loc dir in

  if not (Hashtbl.mem state.visited new_loc) then
    next_machine dir mach :: mach_list
  else mach_list

let get_output mach =
  let rec loop m =
    let m = Intcode.step m in

    match Intcode.get_state m with
    | RUN -> loop m
    | HALT -> (m, None)
    | OUTPUT -> (
        let m, out = Intcode.get_output m in
        match out with
        | None -> raise @@ Failure (Printf.sprintf "No output")
        | Some v -> (m, Some v))
    | s ->
        raise
        @@ Failure
             (Printf.sprintf "Unhandled state %s" (Intcode.state_to_string s))
  in

  loop mach

let proc_output acc mach out =
  let status = status_of_int out in
  let state = Intcode.get_payload mach in

  Hashtbl.replace state.visited state.loc @@ status_to_piece status;

  let m =
    if status = FOUND_SYS then
      Intcode.set_payload { state with oxygen_sys = Some state.loc } mach
    else mach
  in

  if not (status = HIT_WALL) then m :: acc else acc

let get_neighbors mach =
  let to_try =
    [] |> next_direction mach NORTH |> next_direction mach SOUTH
    |> next_direction mach EAST |> next_direction mach WEST
  in

  List.fold_left
    (fun acc m ->
      let m, out = get_output m in

      match out with
      | None -> raise @@ Failure "No output"
      | Some v -> proc_output acc m v)
    [] to_try

let get_all_neighbors mach_list =
  List.fold_left
    (fun acc m ->
      let neighbors = get_neighbors m in

      if List.length neighbors > 0 then neighbors @ acc else acc)
    [] mach_list

let check_for_sys machs =
  List.fold_left
    (fun acc m ->
      let state = Intcode.get_payload m in
      match state.oxygen_sys with None -> acc | Some _ -> Some m)
    None machs

let search_for_sys mach =
  let rec loop machs count =
    let mach_list = get_all_neighbors machs in
    let sys_mach = check_for_sys mach_list in

    match sys_mach with
    | None ->
        if List.length mach_list = 0 then (None, count)
        else loop mach_list (count + 1)
    | Some mach -> (Some mach, count)
  in

  loop [ mach ] 0
