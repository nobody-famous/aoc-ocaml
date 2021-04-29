let height = 6

let width = 25

let calc_pixel layers row col =
  List.fold_left
    (fun pixel layer -> if pixel <> '2' then pixel else layer.(row).(col))
    '2' layers

let print_img img =
  let rec row_loop row =
    let rec col_loop col =
      if col < Array.length img.(row) then (
        Printf.printf "%c" (if img.(row).(col) = '0' then 'X' else ' ');
        col_loop (col + 1))
    in

    if row < Array.length img then (
      print_endline "";
      col_loop 0;
      row_loop (row + 1))
  in

  row_loop 0

let run file_name =
  let input = Parser.parse_input file_name width height in
  let img = Array.make_matrix height width '2' in

  let rec row_loop row =
    let rec col_loop col =
      if col < Array.length img.(row) then (
        img.(row).(col) <- calc_pixel input row col;
        col_loop (col + 1))
    in

    if row < Array.length img then (
      col_loop 0;
      row_loop (row + 1))
  in

  row_loop 0;

  0
