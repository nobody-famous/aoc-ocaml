open Utils

let find_crosses board =
  Hashtbl.fold
    (fun pt _ acc -> if is_cross pt board then pt :: acc else acc)
    board []

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
  let prog = Intcode.parse_input file_name in
  let state =
    Intcode.new_machine new_state prog |> run_machine |> Intcode.get_payload
  in
  let crosses = find_crosses state.board in

  List.fold_left (fun acc pt -> acc + (pt.row * pt.col)) 0 crosses
