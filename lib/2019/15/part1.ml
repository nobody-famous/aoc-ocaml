open Utils

let check_for_sys machs =
  List.fold_left
    (fun acc m ->
      let state = Intcode.get_payload m in
      match state.oxygen_sys with None -> acc | Some _ -> Some m)
    None machs

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
