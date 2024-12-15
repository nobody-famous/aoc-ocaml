let rec matches_numbers target numbers cur_value =
  match numbers with
  | [] -> target = 0
  | number :: rest ->
      (target mod number = 0 && matches_numbers (target / number) rest cur_value)
      || (target - number >= 0 && matches_numbers (target - number) rest cur_value)

let matches_value target numbers cur_value = matches_numbers target (List.rev numbers) cur_value
let run lines = Utils.solve matches_value lines
