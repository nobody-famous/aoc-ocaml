open Utils

let run lines =
  let prog = Intcode.parse_input lines in

  prog.(0) <- 2;

  Aoc.Utils.IntResult (new_game prog |> run_game |> fun g -> g.score)
