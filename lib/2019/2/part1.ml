open Parser
open Intcode

let run file_name =
  let input = parse_input file_name in
  let m = new_machine input in
  let m = set_addr m 1 12 in
  let m = set_addr m 2 2 in

  get_addr (run_prog m) 0
