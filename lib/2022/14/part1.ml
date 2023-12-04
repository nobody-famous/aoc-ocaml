module G = Aoc.Geo2d

type move = NextPoint of G.point | IntoAbyss | Blocked

let next_y points x y =
  match Hashtbl.find_opt points x with
  | Some ys -> (
      match List.filter (fun new_y -> new_y > y) ys |> List.sort compare with
      | top :: _ -> Some (top - 1)
      | [] -> None)
  | None -> None

let get_move_for_y x old_y ys =
  match ys with
  | [] -> IntoAbyss
  | _ -> if old_y = List.hd ys then Blocked else NextPoint { G.x; G.y = old_y }

let check_point grid x y =
  match Hashtbl.find_opt grid.Utils.points x with
  | Some ys ->
      ys
      |> List.filter (fun item -> item >= y)
      |> List.sort compare
      |> get_move_for_y x y
  | None -> IntoAbyss

let rec drop_grain grid x y =
  match next_y grid.Utils.points x y with
  | Some new_y -> (
      match check_point grid (x - 1) (new_y + 1) with
      | NextPoint p -> drop_grain grid p.x p.y
      | IntoAbyss -> ()
      | Blocked -> (
          match check_point grid (x + 1) (new_y + 1) with
          | NextPoint p -> drop_grain grid p.x p.y
          | IntoAbyss -> ()
          | Blocked ->
              let _ = Utils.add_to_map grid.points { G.x; G.y = new_y } in
              drop_grain grid 500 0))
  | None -> ()

let run lines =
  let grid = Parser.parse_input lines in
  let orig_size = Hashtbl.length grid.points in

  drop_grain grid 500 0;

  Hashtbl.length grid.points - orig_size
