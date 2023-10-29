open Utils
open AocUtils

let find_crosses board =
  Hashtbl.fold
    (fun pt _ acc -> if is_cross pt board then pt :: acc else acc)
    board []

let run_machine mach =
  let rec loop m =
    let m = Intcode.step m in

    match Intcode.get_state m with
    | Halt -> m
    | Run -> loop m
    | HasOutput -> handle_output m |> loop
    | s ->
        let str = Intcode.state_to_string s in
        failwith (Printf.sprintf "Unhandled state %s" str)
  in

  loop mach

let run file_name =
  let state =
    Intcode.parse_input file_name
    |> Intcode.new_machine new_state
    |> run_machine |> Intcode.get_payload
  in

  find_crosses state.board
  |> List.fold_left (fun acc pt -> acc + (pt.x * pt.y)) 0
