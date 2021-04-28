let run_prog m =
  let rec loop m =
    let m = Intcode.step m in

    match Intcode.get_state m with
    | RUN -> loop m
    | HALT -> m
    | _ -> raise (Failure "Invalid state")
  in

  loop m
