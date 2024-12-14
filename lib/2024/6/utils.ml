type direction = Up | Down | Left | Right

let find_start grid =
  let rec walk_cols start row col grid =
    if (fst start <> -1 && snd start <> -1) || col >= Array.length grid.(0) then
      start
    else if grid.(row).(col) = '^' then
      (row, col)
    else
      walk_cols start row (col + 1) grid
  in

  let rec walk_rows start row grid =
    if (fst start <> -1 && snd start <> -1) || row >= Array.length grid then
      (start, grid)
    else
      walk_rows (walk_cols start row 0 grid) (row + 1) grid
  in

  grid |> walk_rows (-1, -1) 0

let turn_right = function
  | Up -> Right
  | Down -> Left
  | Left -> Up
  | Right -> Down

let on_grid row col grid = row >= 0 && row < Array.length grid && col >= 0 && col < Array.length grid.(0)

let rec do_steps dir pt grid path seen =
  let vec = (pt, dir) in

  Hashtbl.replace path pt true;

  let handle_step new_row new_col grid =
    Hashtbl.replace seen vec true;

    if not @@ on_grid new_row new_col grid then
      path
    else if grid.(new_row).(new_col) = '#' then
      do_steps (turn_right dir) pt grid path seen
    else
      do_steps dir (new_row, new_col) grid path seen
  in

  match Hashtbl.find_opt seen vec with
  | Some _ -> Hashtbl.create 16
  | None -> (
      let row = fst pt in
      let col = snd pt in

      match dir with
      | Up -> handle_step (row - 1) col grid
      | Down -> handle_step (row + 1) col grid
      | Left -> handle_step row (col - 1) grid
      | Right -> handle_step row (col + 1) grid)

let walk_path (start, grid) = do_steps Up start grid (Hashtbl.create 16) (Hashtbl.create 16)

let get_row_diff = function
  | Up -> -1
  | Down -> 1
  | Right -> 0
  | Left -> 0

let get_col_diff = function
  | Up -> 0
  | Down -> 0
  | Right -> 1
  | Left -> -1

let rec find_wall pt row_diff col_diff grid =
  if not (on_grid (fst pt) (snd pt) grid) then
    (-1, -1)
  else if grid.(fst pt).(snd pt) = '#' then
    (fst pt - row_diff, snd pt - col_diff)
  else
    find_wall (fst pt + row_diff, snd pt + col_diff) row_diff col_diff grid

let rec find_loop seen dir pt grid =
  match Hashtbl.find_opt seen (pt, dir) with
  | Some _ -> true
  | None ->
      if on_grid (fst pt) (snd pt) grid then (
        Hashtbl.replace seen (pt, dir) true;

        let new_dir = turn_right dir in
        let new_pt = find_wall pt (get_row_diff new_dir) (get_col_diff new_dir) grid in

        find_loop seen new_dir new_pt grid)
      else
        false

let is_loop start grid =
  find_loop (Hashtbl.create 16) Up (find_wall start (get_row_diff Up) (get_col_diff Up) grid) grid
