open Printf
open Parser
open Utils
open Types

type steps = { wire1 : int; wire2 : int }

let point_on_line point line =
  if point.x = line.p1.x && in_range point.y line.p1.y line.p2.y then true
  else if point.y = line.p1.y && in_range point.x line.p1.x line.p2.x then true
  else false

let coll_on_line colls line =
  let rec helper c =
    match c with
    | [] -> None
    | point :: rest ->
        if point_on_line point line then Some point else helper rest
  in

  helper colls

let walk wire colls =
  let hmap = Hashtbl.create (List.length colls) in
  let rec helper rem dist =
    match rem with
    | [] -> hmap
    | line :: rest ->
        (match coll_on_line colls line with
        | Some point -> Hashtbl.add hmap point (dist + man_dist line.p1 point)
        | None -> ());
        helper rest (dist + man_dist line.p1 line.p2)
  in

  helper wire 0

let run file_name =
  let wire1, wire2 = parse_input file_name in
  let colls = collisions wire1 wire2 in
  let wire1_dists = walk wire1 colls in
  let wire2_dists = walk wire2 colls in

  print_endline "Wire 1";
  Hashtbl.iter (fun k v -> printf "%d,%d %d\n" k.x k.y v) wire1_dists;

  print_endline "Wire 2";
  Hashtbl.iter (fun k v -> printf "%d,%d %d\n" k.x k.y v) wire2_dists
