module G = Aoc.Geo2d

type grid = {
  min_pt : G.point;
  max_pt : G.point;
  points : (G.point, bool) Hashtbl.t;
}

let to_point input =
  match input with
  | [ fst; snd ] -> { G.x = fst; G.y = snd }
  | _ -> failwith "Invalid point"

let parse_point input =
  input |> String.split_on_char ',' |> List.map int_of_string |> to_point

let fill_points points =
  let rec do_fill (pt : G.point) (rest : G.point list) acc =
    match rest with
    | top :: rem when pt.y < top.y ->
        do_fill { pt with G.y = pt.y + 1 } (top :: rem) (pt :: acc)
    | top :: rem when pt.y > top.y ->
        do_fill { pt with G.y = pt.y - 1 } (top :: rem) (pt :: acc)
    | top :: rem when pt.x > top.x ->
        do_fill { pt with G.x = pt.x - 1 } (top :: rem) (pt :: acc)
    | top :: rem when pt.x < top.x ->
        do_fill { pt with G.x = pt.x + 1 } (top :: rem) (pt :: acc)
    | top :: [] -> top :: acc
    | top :: rem -> do_fill top rem acc
    | _ -> acc
  in

  match points with
  | top :: rest -> do_fill top rest []
  | _ -> points

let parse_line line =
  Str.split (Str.regexp "->") line
  |> List.map String.trim
  |> List.map parse_point
  |> fill_points

let add_to_map map pt =
  Hashtbl.add map pt true;
  map

let to_grid points =
  let min_pt, max_pt = G.get_bounds points in
  { min_pt; max_pt; points }

let parse_input lines =
  lines
  |> List.map parse_line
  |> List.flatten
  |> List.fold_left add_to_map (Hashtbl.create 64)
  |> to_grid
