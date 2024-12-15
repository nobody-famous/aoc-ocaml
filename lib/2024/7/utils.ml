let eq_is_true matches_value (target, numbers) =
  Printf.printf "***** NUMBERS";
  List.iter (fun n -> Printf.printf " %d" n) numbers;
  Printf.printf "\n";
  matches_value target numbers 0

let get_answer matches_value eqs =
  Aoc.Utils.IntResult (eqs |> List.filter (eq_is_true matches_value) |> List.fold_left (fun acc (v, _) -> acc + v) 0)

let solve matches_value lines = lines |> Parser.parse_input |> get_answer matches_value
