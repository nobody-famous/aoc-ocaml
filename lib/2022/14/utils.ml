module G = Aoc.Geo2d

type grid = {
  min_pt : G.point;
  max_pt : G.point;
  points : (int, int list) Hashtbl.t;
}

let print_grid grid =
  let xs =
    List.init (grid.max_pt.x - grid.min_pt.x + 1) (( + ) grid.min_pt.x)
  in
  let ys =
    List.init (grid.max_pt.y - grid.min_pt.y + 1) (( + ) grid.min_pt.y)
  in

  let print_cell y x =
    let ch =
      match Hashtbl.find_opt grid.points x with
      | Some ys -> if List.exists (fun n -> n = y) ys then 'O' else '.'
      | None -> '.'
    in
    Printf.printf "%c" ch
  in

  let print_row y =
    List.iter (print_cell y) xs;
    Printf.printf "\n"
  in

  List.iter print_row ys
