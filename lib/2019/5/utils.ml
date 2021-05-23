let run_prog input m =
  let rec loop m output =
    let m = Intcode.step m in

    match Intcode.get_state m with
    | RUN -> loop m output
    | HALT -> output
    | INPUT ->
        let m = Intcode.set_input input m in
        loop m output
    | OUTPUT ->
        let m, out = Intcode.get_output m in
        loop m out
  in

  loop m None
