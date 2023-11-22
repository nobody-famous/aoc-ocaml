open Utils

let run lines =
  let prog = Intcode.parse_input lines in

  prog.(0) <- 2;

  let game = new_game prog |> run_game in

  game.score
