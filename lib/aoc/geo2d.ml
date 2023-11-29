type point = { x : int; y : int }

let get_bounds points =
  let update_bounds (pt : point) _ (min_pt, max_pt) =
    let min_x = min min_pt.x pt.x in
    let max_x = max max_pt.x pt.x in
    let min_y = min min_pt.y pt.y in
    let max_y = max max_pt.y pt.y in

    let new_min =
      if min_x <> min_pt.x || min_y <> min_pt.y then { x = min_x; y = min_y }
      else min_pt
    in

    let new_max =
      if max_x <> max_pt.x || max_y <> max_pt.y then { x = max_x; y = max_y }
      else max_pt
    in

    (new_min, new_max)
  in

  Hashtbl.fold update_bounds points
    ({ x = max_int; y = max_int }, { x = min_int; y = min_int })
