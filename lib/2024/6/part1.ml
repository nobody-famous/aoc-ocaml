type direction = Up | Down | Left | Right

let turn_right = function
  | Up -> Right
  | Down -> Left
  | Left -> Up
  | Right -> Down

let find_start grid =
  Hashtbl.fold
    (fun k v acc ->
      match acc with
      | Some v, g -> (Some v, g)
      | None, g -> if v = '^' then (Some k, g) else (None, g))
    grid (None, grid)

let rec do_steps dir (row, col) grid path =
  (match Hashtbl.find_opt path (row, col) with
  | None -> Hashtbl.replace path (row, col) true
  | Some _ -> ());

  let handle_step new_row new_col grid =
    match Hashtbl.find_opt grid (new_row, new_col) with
    | None -> path
    | Some v ->
        if v = '#' then
          do_steps (turn_right dir) (row, col) grid path
        else
          do_steps dir (new_row, new_col) grid path
  in

  match dir with
  | Up -> handle_step (row - 1) col grid
  | Down -> handle_step (row + 1) col grid
  | Left -> handle_step row (col - 1) grid
  | Right -> handle_step row (col + 1) grid

let walk_path (start, grid) =
  match start with
  | None -> Hashtbl.create 16
  | Some (row, col) -> do_steps Up (row, col) grid (Hashtbl.create 16)

let run lines = lines |> Parser.parse |> find_start |> walk_path |> Hashtbl.length |> fun len -> Aoc.Utils.IntResult len
