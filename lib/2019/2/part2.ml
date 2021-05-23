open Utils

let run_machine m noun verb =
  m |> Intcode.set_addr 1 noun |> Intcode.set_addr 2 verb |> run_prog

let find_values input low high target =
  let rec outer_loop noun noun_max =
    let rec inner_loop verb verb_max =
      let m = Intcode.new_machine () (Array.copy input) in
      let value = Intcode.get_addr 0 (run_machine m noun verb) in

      if value = target then Some (noun, verb)
      else if verb = verb_max then None
      else inner_loop (verb + 1) verb_max
    in

    match inner_loop low high with
    | Some v -> Some v
    | _ -> if noun = noun_max then None else outer_loop (noun + 1) noun_max
  in

  outer_loop low high

let run file_name =
  let input = Intcode.parse_input file_name in
  let target = 19690720 in

  match find_values input 0 99 target with
  | Some (noun, verb) -> (noun * 100) + verb
  | None -> 0
