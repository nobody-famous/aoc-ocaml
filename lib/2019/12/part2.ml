open Utils

let steps moons =
  let rec loop count ms =
    let stop = List.fold_left (fun acc m -> acc && all_cycles m) true moons in

    if stop then ms
    else
      let ms' = List.map (fun m -> compute_vel m ms) ms in
      let ms' = List.map (fun m -> apply_vel m) ms' in

      loop (count + 1) ms'
  in

  loop 0 moons

let run file_name =
  let moons = Parser.parse_input file_name in

  let moons = steps moons in

  let cycles =
    List.fold_left
      (fun acc m ->
        let x_cycle = match m.cycles.(0).size with None -> 0 | Some s -> s in
        let y_cycle = match m.cycles.(1).size with None -> 0 | Some s -> s in
        let z_cycle = match m.cycles.(2).size with None -> 0 | Some s -> s in

        acc.(0) <- max acc.(0) x_cycle;
        acc.(1) <- max acc.(1) y_cycle;
        acc.(2) <- max acc.(2) z_cycle;

        acc)
      [| 0; 0; 0 |] moons
  in

  Printf.printf "cycles %d %d %d\n" cycles.(0) cycles.(1) cycles.(2);

  List.iter
    (fun m ->
      let x_cycle = match m.cycles.(0).size with Some s -> s | None -> 0 in
      let y_cycle = match m.cycles.(1).size with Some s -> s | None -> 0 in
      let z_cycle = match m.cycles.(2).size with Some s -> s | None -> 0 in
      Printf.printf "x %d y %d z %d\n" x_cycle y_cycle z_cycle)
    moons;
  ()
