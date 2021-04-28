(* type stats = { zeroes : int; ones : int; twos : int } *)

let print_matrix m =
  let rec row_loop row =
    let rec col_loop col =
      if col < Array.length m.(row) then (
        Printf.printf "%c" m.(row).(col);
        col_loop (col + 1))
    in

    if row < Array.length m then (
      col_loop 0;
      print_endline "";
      row_loop (row + 1))
  in

  row_loop 0;
  0

let run file_name =
  let img = Parser.parse_input file_name 3 2 in
  Printf.printf "Layers %d\n" (List.length img);

  let _ = List.fold_left (fun _ m -> print_matrix m) 0 img in
  0
