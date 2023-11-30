module G = Aoc.Geo2d

type move = NextPoint of G.point | IntoAbyss | Blocked

let next_y points x y =
  match Hashtbl.find_opt points x with
  | Some ys -> (
      match List.filter (fun new_y -> new_y > y) ys |> List.sort compare with
      | top :: _ -> Some (top - 1)
      | [] -> None)
  | None -> None

let run lines =
  let data = Parser.parse_input lines in
  Printf.printf "min %d,%d\n" data.min_pt.x data.min_pt.y;
  Printf.printf "max %d,%d\n" data.max_pt.x data.max_pt.y;
  Printf.printf "size %d\n" @@ Hashtbl.length data.points;

  Utils.print_grid data;

  (match next_y data.points 500 0 with
  | Some y -> Printf.printf "new y: %d\n" y
  | None -> Printf.printf "No new y\n");

  0
