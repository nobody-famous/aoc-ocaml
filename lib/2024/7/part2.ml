let concat_ints a b = string_of_int a ^ string_of_int b |> int_of_string

let rec matches_value target numbers cur_value =
  match numbers with
  | [] -> target = cur_value
  | number :: rest ->
      matches_value target rest (cur_value + number)
      || matches_value target rest (cur_value * number)
      || matches_value target rest (concat_ints cur_value number)

let run lines = Utils.solve matches_value lines
