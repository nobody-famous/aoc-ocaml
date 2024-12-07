let inc_value k m =
  (match Hashtbl.find_opt m k with
  | None -> Hashtbl.replace m k k
  | Some old -> Hashtbl.replace m k (k + old));
  m

let get_scores (left, right) =
  (left, List.fold_left (fun scores item -> inc_value item scores) (Hashtbl.create 16) right)

let get_score k scores =
  match Hashtbl.find_opt scores k with
  | None -> 0
  | Some v -> v

let calculate_answer (left, scores) = List.fold_left (fun acc item -> acc + get_score item scores) 0 left
let run lines = Aoc.Utils.IntResult (lines |> Parser.parse_input |> get_scores |> calculate_answer)
