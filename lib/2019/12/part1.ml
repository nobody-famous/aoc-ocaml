open Utils

let run file_name =
  let input = Parser.parse_input file_name in

  List.iter (fun m -> Printf.printf "%s\n" (moon_to_string m)) input
