open Parser

let deltas_to_lines deltas =
  let rec foo p d lines =
    match d with
    | [] -> List.rev lines
    | h :: t ->
        let p' = { x = p.x + h.x; y = p.y + h.y } in
        foo p' t ({ p1 = p; p2 = p' } :: lines)
  in

  foo { x = 0; y = 0 } deltas []

let is_horiz line = line.p1.y = line.p2.y

let is_vert line = line.p1.x = line.p2.x

let in_range value a b =
  if a < b then value >= a && value <= b else value >= b && value <= a

let lines_cross horiz vert =
  in_range vert.p1.x horiz.p1.x horiz.p2.x
  && in_range horiz.p1.y vert.p1.y vert.p2.y

let collision line1 line2 =
  if is_vert line1 && is_horiz line2 && lines_cross line2 line1 then
    Some { x = line1.p1.x; y = line2.p1.y }
  else if is_horiz line1 && is_vert line2 && lines_cross line1 line2 then
    Some { x = line2.p1.x; y = line1.p1.y }
  else None

let match_lines line lines acc =
  let rec loop rem acc =
    match rem with
    | [] -> acc
    | h :: t -> (
        match collision line h with
        | Some c when c.x <> 0 && c.y <> 0 -> loop t (c :: acc)
        | _ -> loop t acc)
  in

  loop lines acc

let collisions wire1 wire2 =
  let rec i_loop i_lines acc =
    match i_lines with
    | [] -> acc
    | i_h :: t ->
        let acc' = match_lines i_h wire2 acc in
        i_loop t acc'
  in

  i_loop wire1 []

let diff a b = Int.abs (a - b)

let man_dist p1 p2 = diff p1.x p2.x + diff p1.y p2.y

let run file_name =
  let deltas = parse_input file_name in
  let wires = List.map deltas_to_lines deltas in
  let wire1, wire2 = (List.nth wires 0, List.nth wires 1) in
  let colls = collisions wire1 wire2 in
  let dists = List.map (fun c -> man_dist { x = 0; y = 0 } c) colls in

  List.fold_left Stdlib.min Int.max_int dists
