(* let run lines = Aoc.Utils.IntResult (lines |> Parser.parse |> Utils.find_start |> Utils.walk_path |> Utils.Points.cardinal) *)
let run lines =
  let start = lines |> Parser.parse |> Utils.find_start in
  Printf.printf "***** START %d,%d\n" (fst start) (snd start);
  Aoc.Utils.IntResult 0
