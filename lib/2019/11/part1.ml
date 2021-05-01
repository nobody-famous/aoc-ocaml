open Utils

let run file_name =
  let prog = Intcode.parse_input file_name in
  let bot = new_robot prog in

  run_robot bot BLACK;

  Hashtbl.length bot.seen
