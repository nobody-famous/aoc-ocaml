type direction = NORTH | SOUTH | EAST | WEST

type status = HIT_WALL | MOVED | FOUND_SYS

type piece = WALL | EMPTY | OXYGEN_SYS | UNKNOWN

type point = { x : int; y : int }

let status_of_int = function
  | 0 -> HIT_WALL
  | 1 -> MOVED
  | 2 -> FOUND_SYS
  | s -> raise @@ Failure (Printf.sprintf "Invalid status code %d" s)

let piece_to_string = function
  | WALL -> "WALL"
  | EMPTY -> "EMPTY"
  | OXYGEN_SYS -> "OXYGEN_SYS"
  | UNKNOWN -> "UNKNOWN"

let status_to_piece = function
  | HIT_WALL -> WALL
  | MOVED -> EMPTY
  | FOUND_SYS -> OXYGEN_SYS

let status_to_string = function
  | HIT_WALL -> "WALL"
  | MOVED -> "EMPTY"
  | FOUND_SYS -> "OXYGEN_SYS"

let dir_to_int dir =
  match dir with NORTH -> 1 | SOUTH -> 2 | WEST -> 3 | EAST -> 4

let dir_to_opp dir =
  match dir with NORTH -> SOUTH | SOUTH -> NORTH | WEST -> EAST | EAST -> WEST

let dir_to_string m =
  match m with
  | NORTH -> "NORTH"
  | SOUTH -> "SOUTH"
  | EAST -> "EAST"
  | WEST -> "WEST"

let next_move dir =
  match dir with NORTH -> EAST | SOUTH -> WEST | EAST -> SOUTH | WEST -> NORTH

let print_board board =
  let low_x, high_x, low_y, high_y =
    Hashtbl.fold
      (fun k _ (lx, hx, ly, hy) ->
        ( Stdlib.min lx k.x,
          Stdlib.max hx k.x,
          Stdlib.min ly k.y,
          Stdlib.max hy k.y ))
      board
      (max_int, min_int, max_int, min_int)
  in

  let rec y_loop y =
    let rec x_loop x =
      let piece = try Hashtbl.find board { x; y } with Not_found -> UNKNOWN in

      let ch =
        match piece with
        | EMPTY -> ' '
        | WALL -> '#'
        | OXYGEN_SYS -> 'X'
        | UNKNOWN -> '?'
      in

      let ch = if x = 0 && y = 0 then 'S' else ch in

      Printf.printf "%c" ch;
      if x <= high_x then x_loop (x + 1)
    in

    Printf.printf "?";
    x_loop low_x;
    Printf.printf "?\n";
    if y >= low_y then y_loop (y - 1)
  in

  y_loop high_y
