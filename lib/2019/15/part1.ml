open Utils

type search_state = {
  loc : point;
  oxygen_sys : point option;
  path : direction list;
  visited : (point, piece) Hashtbl.t;
}

let new_state =
  let state =
    {
      loc = { x = 0; y = 0 };
      oxygen_sys = None;
      path = [];
      visited = Hashtbl.create 64;
    }
  in

  Hashtbl.replace state.visited state.loc EMPTY;
  state

let move_loc loc dir =
  match dir with
  | NORTH -> { loc with y = loc.y + 1 }
  | SOUTH -> { loc with y = loc.y - 1 }
  | EAST -> { loc with x = loc.x + 1 }
  | WEST -> { loc with x = loc.x - 1 }

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

let check_for_sys machs =
  List.fold_left
    (fun acc m ->
      let state = Intcode.get_payload m in
      match state.oxygen_sys with None -> acc | Some _ -> Some m)
    None machs

let get_all_neighbors mach_list =
  List.fold_left
    (fun acc m ->
      let neighbors = get_neighbors m in

      if List.length neighbors > 0 then neighbors @ acc else acc)
    [] mach_list

let search_for_sys prog =
  let rec loop machs =
    let mach_list = get_all_neighbors machs in
    let sys_mach = check_for_sys mach_list in

    match sys_mach with
    | None ->
        if List.length mach_list = 0 then raise @@ Failure "Machine list empty"
        else loop mach_list
    | Some mach -> mach
  in

  let mach =
    Intcode.new_machine new_state prog |> Intcode.set_input (dir_to_int NORTH)
  in

  loop [ mach ]

let run file_name =
  let prog = Intcode.parse_input file_name in
  let mach = search_for_sys prog in
  let state = Intcode.get_payload mach in

  List.length state.path
