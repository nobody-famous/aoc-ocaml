module G = Aoc.Geo2d

let to_point input =
  match input with
  | [ fst; snd ] -> { G.x = fst; G.y = snd }
  | _ -> failwith "Invalid point"

let parse_point input =
  input |> String.split_on_char ',' |> List.map int_of_string |> to_point

let fill_points points =
  let rec do_fill pt rest acc =
    match rest with
    | top :: rem when pt.G.y < top.G.y ->
        do_fill { pt with G.y = pt.G.y + 1 } (top :: rem) (pt :: acc)
    | top :: rem when pt.G.y > top.G.y ->
        do_fill { pt with G.y = pt.G.y - 1 } (top :: rem) (pt :: acc)
    | top :: rem when pt.G.x > top.G.x ->
        do_fill { pt with G.x = pt.G.x - 1 } (top :: rem) (pt :: acc)
    | top :: rem when pt.G.x < top.G.x ->
        do_fill { pt with G.x = pt.G.x + 1 } (top :: rem) (pt :: acc)
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
  (match Hashtbl.find_opt map pt.G.x with
  | Some ys -> Hashtbl.add map pt.G.x (pt.G.y :: ys)
  | None -> Hashtbl.add map pt.G.x [ pt.G.y ]);
  map

let to_grid points =
  let min_pt, max_pt =
    Hashtbl.fold
      (fun x ys (min_pt, max_pt) ->
        ( { G.x = min x min_pt.G.x; G.y = List.fold_left min min_pt.G.y ys },
          { G.x = max x max_pt.G.x; G.y = List.fold_left max max_pt.G.y ys } ))
      points
      ({ G.x = max_int; G.y = max_int }, { G.x = min_int; G.y = min_int })
  in

  { Utils.min_pt; Utils.max_pt; Utils.points }

let parse_input lines =
  lines
  |> List.map parse_line
  |> List.flatten
  |> List.fold_left add_to_map (Hashtbl.create 64)
  |> to_grid
