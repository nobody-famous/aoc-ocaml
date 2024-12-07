let sort (left, right) = (List.sort compare left, List.sort compare right)

let rec calculate_answer ans (left, right) =
  match (left, right) with
  | x :: rest_left, y :: rest_right -> calculate_answer (ans + abs (x - y)) (rest_left, rest_right)
  | [], [] -> Aoc.Utils.IntResult ans
  | _, [] -> failwith "Ran out of right items"
  | [], _ -> failwith "Ran out of left items"

let run lines = lines |> Parser.parse_input |> sort |> calculate_answer 0
