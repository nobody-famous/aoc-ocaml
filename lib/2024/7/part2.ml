let check_ends_with a b = String.ends_with ~suffix:(string_of_int b) (string_of_int a)

let strip_suffix a b =
  let a_str = string_of_int a in
  let b_str = string_of_int b in
  int_of_string @@ String.sub a_str 0 (String.length a_str - String.length b_str)

let rec matches_numbers target numbers cur_value =
  match numbers with
  | [] -> target = 0
  | number :: rest ->
      (target mod number = 0 && matches_numbers (target / number) rest cur_value)
      || (target - number >= 0 && matches_numbers (target - number) rest cur_value)
      || (check_ends_with target number && matches_numbers (strip_suffix target number) rest cur_value)

let matches_value target numbers cur_value = matches_numbers target (List.rev numbers) cur_value
let run lines = Utils.solve matches_value lines
