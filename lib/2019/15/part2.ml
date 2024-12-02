open Utils

let find_oxygen_sys prog =
  prog |> Intcode.new_machine (new_state { x = 0; y = 0 }) |> Intcode.set_input (dir_to_int NORTH) |> search_for_sys

let reset_state mach =
  let state = Intcode.get_payload mach in

  Hashtbl.clear state.visited;
  Hashtbl.replace state.visited state.loc EMPTY;

  Intcode.set_payload { state with oxygen_sys = None } mach

let run lines =
  let mach, _ = Intcode.parse_input lines |> find_oxygen_sys in

  Aoc.Utils.IntResult
    (match mach with
    | None -> 0
    | Some m ->
        let m = reset_state m in
        let _, count = search_for_sys m in
        count)
