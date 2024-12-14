let run lines = Aoc.Utils.IntResult (lines |> Parser.parse |> Utils.find_start |> Utils.walk_path |> Hashtbl.length)
