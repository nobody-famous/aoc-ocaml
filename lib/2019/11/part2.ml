open Utils

type bounds = { min_row : int; max_row : int; min_col : int; max_col : int }

let get_bounds seen =
  Hashtbl.fold
    (fun loc _ acc ->
      {
        min_row = Stdlib.min loc.row acc.min_row;
        max_row = Stdlib.max loc.row acc.max_row;
        min_col = Stdlib.min loc.col acc.min_col;
        max_col = Stdlib.max loc.col acc.max_row;
      })
    seen
    {
      min_row = max_int;
      max_row = min_int;
      min_col = max_int;
      max_col = min_int;
    }

let print_grid bot =
  let bounds = get_bounds bot.seen in
  let rec row_loop row =
    let rec col_loop col =
      if col < bounds.max_col then (
        let loc = { row; col } in
        let color = try Hashtbl.find bot.seen loc with Not_found -> BLACK in

        Printf.printf "%c"
          (match color with
          | BLACK -> ' '
          | WHITE -> 'X');

        col_loop (col + 1))
    in

    col_loop bounds.min_col;
    Printf.printf "\n";
    if row < bounds.max_row then row_loop (row + 1)
  in

  row_loop bounds.min_row

let run lines =
  let bot = Intcode.parse_input lines |> new_robot in

  run_robot bot WHITE;

  0
