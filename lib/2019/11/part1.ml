open Utils

let run file_name =
  let bot = Intcode.parse_input file_name |> new_robot in

  run_robot bot BLACK;

  Hashtbl.length bot.seen
