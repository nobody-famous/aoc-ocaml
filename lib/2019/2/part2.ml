open Parser
open Intcode

let run_machine m noun verb =
  let m = set_addr m 1 noun in
  let m = set_addr m 2 verb in

  run_prog m

let find_values input low high target =
  let rec outer_loop noun noun_max =
    let rec inner_loop verb verb_max =
      let m = new_machine (Array.copy input) in
      let value = get_addr (run_machine m noun verb) 0 in

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
  let input = parse_input file_name in
  let target = 19690720 in

  match find_values input 0 99 target with
  | Some (noun, verb) -> (noun * 100) + verb
  | None -> 0
