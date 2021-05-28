type point = { x : int; y : int }

type point_3d = { x3 : int; y3 : int; z3 : int }

let point_to_string pt = Printf.sprintf "(%d,%d)" pt.x pt.y

type board_bounds = { low_x : int; high_x : int; low_y : int; high_y : int }

let new_bounds =
  { low_x = max_int; high_x = min_int; low_y = max_int; high_y = min_int }

let get_board_bounds board =
  Hashtbl.fold
    (fun pt _ acc ->
      {
        low_x = min acc.low_x pt.x;
        high_x = max acc.high_x pt.x;
        low_y = min acc.low_y pt.y;
        high_y = max acc.high_y pt.y;
      })
    board new_bounds
