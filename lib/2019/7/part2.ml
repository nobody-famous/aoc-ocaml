open Utils

let run file_name =
  let _ = Intcode.parse_input file_name in
  let _ = permutations [| 5; 6; 7; 8; 9 |] in

  0
