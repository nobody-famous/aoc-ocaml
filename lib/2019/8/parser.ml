type image = int array array list

let parse_input file_name width height =
  let line = InputParser.read_lines file_name |> InputParser.get_first_line in

  let rec loop ndx img =
    let matrix = Array.make_matrix height width '0' in

    let rec row_loop row row_ndx =
      let rec col_loop col col_ndx =
        if col < width then (
          matrix.(row).(col) <- String.get line col_ndx;
          col_loop (col + 1) (col_ndx + 1))
        else col_ndx
      in

      if row < height then
        let ndx' = col_loop 0 row_ndx in
        row_loop (row + 1) ndx'
      else row_ndx
    in

    let ndx' = row_loop 0 ndx in
    let img' = matrix :: img in

    if ndx' < String.length line then loop ndx' img' else List.rev img'
  in

  loop 0 []
