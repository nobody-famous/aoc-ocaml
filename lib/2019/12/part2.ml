open Utils

let steps moons =
  let rec loop ms =
    let stop = List.fold_left (fun acc m -> acc && all_cycles m) true moons in

    if stop then ms
    else
      ms
      |> List.map (fun m -> compute_vel m ms)
      |> List.map (fun m -> apply_vel m)
      |> loop
  in

  loop moons

let get_cycles moons =
  List.fold_left
    (fun acc m ->
      let x_cycle =
        match m.cycles.(0).size with
        | None -> 0
        | Some s -> s
      in
      let y_cycle =
        match m.cycles.(1).size with
        | None -> 0
        | Some s -> s
      in
      let z_cycle =
        match m.cycles.(2).size with
        | None -> 0
        | Some s -> s
      in

      acc.(0) <- max acc.(0) x_cycle;
      acc.(1) <- max acc.(1) y_cycle;
      acc.(2) <- max acc.(2) z_cycle;

      acc)
    [| 0; 0; 0 |] moons

let rec gcd m n = if n <> 0 then gcd n (m mod n) else abs m

let lcm m n =
  match (m, n) with
  | 0, _ | _, 0 -> 0
  | m, n -> abs (m * n) / gcd m n

let run lines =
  Parser.parse_input lines
  |> steps
  |> get_cycles
  |> Array.fold_left (fun acc c -> lcm acc c) 1
