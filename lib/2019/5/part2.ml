open Utils

let run file_name =
  let output =
    Intcode.parse_input file_name |> Intcode.new_machine () |> run_prog 5
  in

  match output with
  | None -> 0
  | Some out -> out
