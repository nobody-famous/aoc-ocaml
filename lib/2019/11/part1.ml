open Utils

let run lines =
  let bot = Intcode.parse_input lines |> new_robot in

  run_robot bot BLACK;

  Hashtbl.length bot.seen
