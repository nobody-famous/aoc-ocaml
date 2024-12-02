open Utils

let run lines =
  let mach, _ =
    Intcode.parse_input lines
    |> Intcode.new_machine (new_state { x = 0; y = 0 })
    |> Intcode.set_input (dir_to_int NORTH)
    |> search_for_sys
  in

  Aoc.Utils.IntResult
    (match mach with
    | None -> 0
    | Some m ->
        let state = Intcode.get_payload m in
        List.length state.path)
