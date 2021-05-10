open Utils

let run_machine mach =
  let rec loop m =
    let m = Intcode.step m in

    match Intcode.get_state m with
    | HALT -> m
    | RUN -> loop m
    | OUTPUT -> handle_output m |> loop
    | s ->
        let str = Intcode.state_to_string s in
        raise @@ Failure (Printf.sprintf "Unhandled state %s" str)
  in

  loop mach

let run file_name =
  let state =
    Intcode.parse_input file_name
    |> Intcode.new_machine new_state
    |> run_machine |> Intcode.get_payload
  in

  print_board state.board;
  0
