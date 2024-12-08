module Point = struct
  type t = int * int

  let compare (a_row, a_col) (b_row, b_col) =
    if a_row < b_row || (a_row = b_row && a_col < b_col) then -1 else if a_row = b_row && a_col = b_col then 0 else 1
end

type direction = Up | Down | Left | Right

module Vector = struct
  type t = Point.t * direction

  let compare (a_pt, a_dir) (b_pt, b_dir) =
    match a_dir with
    | Up -> (
        match b_dir with
        | Up -> Point.compare a_pt b_pt
        | Down -> -1
        | Left -> -1
        | Right -> -1)
    | Down -> (
        match b_dir with
        | Up -> 1
        | Down -> Point.compare a_pt b_pt
        | Left -> -1
        | Right -> -1)
    | Left -> (
        match b_dir with
        | Up -> 1
        | Down -> 1
        | Left -> Point.compare a_pt b_pt
        | Right -> -1)
    | Right -> (
        match b_dir with
        | Up -> 1
        | Down -> 1
        | Left -> 1
        | Right -> Point.compare a_pt b_pt)
end

module Points = Set.Make (Point)
module Grid = Map.Make (Point)
module Seen = Set.Make (Vector)

let find_start grid =
  Grid.fold
    (fun k v acc ->
      match acc with
      | (-1, -1), g -> if v = '^' then (k, g) else acc
      | _, _ -> acc)
    grid
    ((-1, -1), grid)

let turn_right = function
  | Up -> Right
  | Down -> Left
  | Left -> Up
  | Right -> Down

let rec do_steps dir (row, col) grid path seen =
  let new_path = Points.add (row, col) path in
  let vec = ((row, col), dir) in

  let handle_step new_row new_col grid =
    let new_seen = Seen.add vec seen in

    match Grid.find_opt (new_row, new_col) grid with
    | None -> new_path
    | Some v ->
        if v = '#' then
          do_steps (turn_right dir) (row, col) grid new_path new_seen
        else
          do_steps dir (new_row, new_col) grid new_path new_seen
  in

  match Seen.find_opt vec seen with
  | Some _ -> Points.empty
  | None -> (
      match dir with
      | Up -> handle_step (row - 1) col grid
      | Down -> handle_step (row + 1) col grid
      | Left -> handle_step row (col - 1) grid
      | Right -> handle_step row (col + 1) grid)

let walk_path ((row, col), grid) = do_steps Up (row, col) grid Points.empty Seen.empty
