let rec matches_value target numbers cur_value =
  match numbers with
  | number :: rest -> matches_value target rest (cur_value + number) || matches_value target rest (cur_value * number)
  | [] -> target = cur_value

let run lines = Utils.solve matches_value lines
