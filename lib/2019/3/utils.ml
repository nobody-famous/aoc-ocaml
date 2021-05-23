open Types

let is_horiz line = line.p1.y = line.p2.y

let is_vert line = line.p1.x = line.p2.x

let in_range value a b =
  if a < b then value >= a && value <= b else value >= b && value <= a

let lines_cross horiz vert =
  in_range vert.p1.x horiz.p1.x horiz.p2.x
  && in_range horiz.p1.y vert.p1.y vert.p2.y

let diff a b = Int.abs (a - b)

let man_dist p1 p2 = diff p1.x p2.x + diff p1.y p2.y

let collision line1 line2 =
  if is_vert line1 && is_horiz line2 && lines_cross line2 line1 then
    Some { x = line1.p1.x; y = line2.p1.y }
  else if is_horiz line1 && is_vert line2 && lines_cross line1 line2 then
    Some { x = line2.p1.x; y = line1.p1.y }
  else None

let collisions_for_line line lines acc =
  let rec loop rem acc =
    match rem with
    | [] -> acc
    | h :: t -> (
        match collision line h with
        | Some c when c.x <> 0 && c.y <> 0 -> loop t (c :: acc)
        | _ -> loop t acc)
  in

  loop lines acc

let collisions (wire1, wire2) =
  let rec i_loop i_lines acc =
    match i_lines with
    | [] -> acc
    | i_h :: t -> i_loop t @@ collisions_for_line i_h wire2 acc
  in

  i_loop wire1 []
