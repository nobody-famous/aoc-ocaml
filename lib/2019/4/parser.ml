open InputParser

let to_int_array input =
  let rec loop str acc =
    match str with
    | "" -> Array.of_list (List.rev acc)
    | s ->
        let int_value = Char.code s.[0] - Char.code '0' in
        let length = String.length s in
        let rest = String.sub s 1 (length - 1) in

        loop rest (int_value :: acc)
  in

  loop input []

let parse_input lines =
  match lines with
  | [] -> ([||], [||])
  | first :: _ ->
      let arrays = String.split_on_char '-' first |> List.map to_int_array in

      (List.nth arrays 0, List.nth arrays 1)
