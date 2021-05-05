open Utils

let run file_name =
  let input = Parser.parse_input file_name in
  let answer = calc_ore input 1 in

  answer
