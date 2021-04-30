open Utils

let find_asteroids lines =
  let rec loop rem_lines row asteroids =
    match rem_lines with
    | [] -> asteroids
    | line :: rest ->
        let rec col_loop col col_asteroids =
          if col < String.length line then
            let col_asteroids' =
              if String.get line col = '#' then
                { x = col; y = row } :: col_asteroids
              else col_asteroids
            in

            col_loop (col + 1) col_asteroids'
          else col_asteroids
        in

        let asteroids' = col_loop 0 asteroids in
        loop rest (row + 1) asteroids'
  in

  loop lines 0 []

let parse_input file_name =
  let lines = InputParser.read_lines file_name in

  find_asteroids lines
