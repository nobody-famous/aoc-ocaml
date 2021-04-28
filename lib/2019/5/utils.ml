let run_prog m input =
  let rec loop m output =
    let m = Intcode.step m in

    match Intcode.get_state m with
    | RUN ->
        let m, out = Intcode.get_output m in
        loop m out
    | HALT -> output
    | INPUT ->
        let m = Intcode.set_input m input in
        loop m output
  in

  loop m None
