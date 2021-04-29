let print_matrix m =
  let rec row_loop row =
    let rec col_loop col =
      if col < Array.length m.(row) then (
        Printf.printf "%c"
          (if m.(row).(col) = Parser.Asteroid then '#' else '.');
        col_loop (col + 1))
    in

    if row < Array.length m then (
      col_loop 0;
      print_endline "";
      row_loop (row + 1))
  in

  row_loop 0

let run file_name =
  let input = Parser.parse_input file_name in

  let ht = Hashtbl.create 16 in

  Hashtbl.replace ht (9. /. 1.) 1;
  Hashtbl.replace ht (5. /. 3.) 1;

  Printf.printf "Found? %b\n" (Hashtbl.mem ht (18. /. 2.));
  Printf.printf "Found? %b\n" (Hashtbl.mem ht (15. /. 9.));
  Printf.printf "size %d\n" (Hashtbl.length ht);

  print_matrix input;
  0
