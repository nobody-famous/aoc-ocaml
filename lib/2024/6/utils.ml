module Point = struct
  type t = int * int
end

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
