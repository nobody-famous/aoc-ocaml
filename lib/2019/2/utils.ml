let run_prog m =
  let rec loop m =
    let m = Intcode.step m in

    match Intcode.get_state m with
    | Run -> loop m
    | Halt -> m
    | _ -> failwith "Invalid state"
  in

  loop m
