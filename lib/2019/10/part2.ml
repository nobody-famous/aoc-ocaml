open Utils

let find_station input =
  let point, _ =
    List.fold_left
      (fun (p, best) p' ->
        let count = count_asteroids p' input in
        if count > best then (p', count) else (p, best))
      ((0, 0), 0)
      input
  in

  point

let run file_name =
  let input = Parser.parse_input file_name in
  let point = find_station input in

  let row, col = point in
  Printf.printf "%d %d\n" row col;
  0
