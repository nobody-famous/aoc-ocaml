let concat_ints a b =
  Printf.printf "***** CONCAT %d %d %s\n" a b (string_of_int a ^ string_of_int b);
  string_of_int a ^ string_of_int b |> int_of_string

let rec matches_value target numbers cur_value =
  Printf.printf "***** MATCHES %d [" cur_value;
  List.iter (fun n -> Printf.printf " %d" n) numbers;
  Printf.printf "]\n";
  match numbers with
  | number :: [] -> target = number || matches_value target [] (concat_ints cur_value number)
  | [] -> target = cur_value
  | number :: rest ->
      matches_value target rest (cur_value + number)
      || matches_value target rest (cur_value * number)
      || matches_value target rest (concat_ints cur_value number)

let run lines = Utils.solve matches_value lines
