type direction = Up | Down | Left | Right

let turn_right = function
  | Up -> Right
  | Down -> Left
  | Left -> Up
  | Right -> Down

let find_start grid =
  Utils.Grid.fold
    (fun k v acc ->
      match acc with
      | Some _, _ -> acc
      | None, g -> if v = '^' then (Some k, g) else acc)
    grid (None, grid)

let rec do_steps dir (row, col) grid path =
  let new_path = Utils.Points.add (row, col) path in

  let handle_step new_row new_col grid =
    match Utils.Grid.find_opt (new_row, new_col) grid with
    | None -> new_path
    | Some v ->
        if v = '#' then
          do_steps (turn_right dir) (row, col) grid new_path
        else
          do_steps dir (new_row, new_col) grid new_path
  in

  match dir with
  | Up -> handle_step (row - 1) col grid
  | Down -> handle_step (row + 1) col grid
  | Left -> handle_step row (col - 1) grid
  | Right -> handle_step row (col + 1) grid

let walk_path (start, grid) =
  match start with
  | None -> Utils.Points.empty
  | Some (row, col) -> do_steps Up (row, col) grid Utils.Points.empty

let run lines = Aoc.Utils.IntResult (lines |> Parser.parse |> find_start |> walk_path |> Utils.Points.cardinal)
