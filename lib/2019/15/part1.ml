open Utils

type search_state = { cur_loc : piece; visited : (point, bool) Hashtbl.t }

type 'a mach_loc = { mach : 'a Intcode.machine; loc : point }

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

let get_neighbors mach =
  let to_try =
    [
      Intcode.set_input (dir_to_int NORTH) mach;
      Intcode.set_input (dir_to_int SOUTH) mach;
      Intcode.set_input (dir_to_int EAST) mach;
      Intcode.set_input (dir_to_int WEST) mach;
    ]
  in

  List.fold_left
    (fun acc m ->
      let m, out = get_output m in

      match out with
      | None -> acc
      | Some v ->
          let status = status_of_int v in
          let m =
            if status = FOUND_SYS then Intcode.set_payload m OXYGEN_SYS else m
          in

          if not (status = HIT_WALL) then m :: acc else acc)
    [] to_try

let get_mach_neighbors m = get_neighbors m

let search_for_sys prog =
  let rec loop machs =
    let mach_list =
      List.fold_left
        (fun acc m ->
          let neighbors = get_mach_neighbors m in

          Printf.printf "neighbors %d\n" @@ List.length neighbors;
          if List.length neighbors > 0 then neighbors @ acc else acc)
        [] machs
    in

    if List.length mach_list > 0 then loop mach_list else ()
  in

  let init =
    {
      loc = { x = 0; y = 0 };
      mach =
        Intcode.new_machine UNKNOWN prog |> Intcode.set_input (dir_to_int NORTH);
    }
  in

  loop [ init ]

let run file_name =
  let prog = Intcode.parse_input file_name in

  let _ = search_for_sys prog in

  0
