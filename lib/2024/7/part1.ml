let rec matches_value target numbers cur_value =
  match numbers with
  | number :: rest -> matches_value target rest (cur_value + number) || matches_value target rest (cur_value * number)
  | [] -> target = cur_value

let eq_is_true (target, numbers) = matches_value target numbers 0
let get_answer eqs = Aoc.Utils.IntResult (eqs |> List.filter eq_is_true |> List.fold_left (fun acc (v, _) -> acc + v) 0)
let run lines = lines |> Parser.parse_input |> get_answer
