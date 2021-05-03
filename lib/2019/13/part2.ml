open Utils

let run file_name =
  let prog = Intcode.parse_input file_name in

  prog.(0) <- 2;

  let game = new_game prog |> run_game in

  game.score
