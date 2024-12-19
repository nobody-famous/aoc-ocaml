type robot = { position : int * int; velocity : int * int }
type quadrant = { top_left : int * int; btm_right : int * int }

let rec do_mod x y = if x > 0 then x mod y else do_mod (x + y) y

let move width height robot =
  let new_x = do_mod (fst robot.position + fst robot.velocity) width in
  let new_y = do_mod (snd robot.position + snd robot.velocity) height in
  { robot with position = (new_x, new_y) }

let rec do_moves width height count robots =
  if count > 0 then do_moves width height (count - 1) (List.map (move width height) robots) else robots

let is_in_quadrant quadrant robot =
  fst robot.position >= fst quadrant.top_left
  && snd robot.position >= snd quadrant.top_left
  && fst robot.position <= fst quadrant.btm_right
  && snd robot.position <= snd quadrant.btm_right

let calculate_quadrants width height =
  [
    { top_left = (0, 0); btm_right = ((width / 2) - 1, (height / 2) - 1) };
    { top_left = ((width / 2) + 1, 0); btm_right = (width - 1, (height / 2) - 1) };
    { top_left = (0, (height / 2) + 1); btm_right = ((width / 2) - 1, height - 1) };
    { top_left = ((width / 2) + 1, (height / 2) + 1); btm_right = (width - 1, height - 1) };
  ]

let get_bounds width height robots = (calculate_quadrants width height, robots)

let count_robots area robots =
  List.fold_left (fun total robot -> if is_in_quadrant area robot then total + 1 else total) 0 robots

let update_quadrant_counts quadrants counts robot =
  match quadrants with
  | [ tl; tr; bl; br ] ->
      if is_in_quadrant tl robot then
        counts.(0) <- counts.(0) + 1
      else if is_in_quadrant tr robot then
        counts.(1) <- counts.(1) + 1
      else if is_in_quadrant bl robot then
        counts.(2) <- counts.(2) + 1
      else if is_in_quadrant br robot then
        counts.(3) <- counts.(3) + 1;
      counts
  | _ -> counts

let calculate_safety (quadrants, robots) =
  robots |> List.fold_left (update_quadrant_counts quadrants) [| 0; 0; 0; 0 |] |> Array.fold_left ( * ) 1
