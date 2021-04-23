open Parser
open Utils
open Types

type steps = { wire1 : int; wire2 : int }

let point_on_line point line =
  (point.x = line.p1.x && in_range point.y line.p1.y line.p2.y)
  || (point.y = line.p1.y && in_range point.x line.p1.x line.p2.x)

let coll_on_line colls line =
  let rec loop c acc =
    match c with
    | [] -> acc
    | point :: rest ->
        if point_on_line point line then loop rest (point :: acc)
        else loop rest acc
  in

  loop colls []

let coll_dists wire colls =
  let hmap = Hashtbl.create (List.length colls) in
  let rec loop rem dist =
    match rem with
    | [] -> hmap
    | line :: rest ->
        let points = coll_on_line colls line in
        let add_map point =
          Hashtbl.add hmap point (dist + man_dist line.p1 point)
        in

        List.iter add_map points;
        loop rest (dist + man_dist line.p1 line.p2)
  in

  loop wire 0

let combine dists_1 dists_2 =
  let rec loop rem acc =
    match rem with
    | [] -> acc
    | (p, d) :: rest ->
        let d' = Hashtbl.find dists_2 p in
        loop rest ((d + d') :: acc)
  in

  let dists_1_list = Hashtbl.fold (fun k v acc -> (k, v) :: acc) dists_1 [] in

  loop dists_1_list []

let run file_name =
  let wire1, wire2 = parse_input file_name in
  let colls = collisions wire1 wire2 in
  let wire1_dists = coll_dists wire1 colls in
  let wire2_dists = coll_dists wire2 colls in
  let combined = combine wire1_dists wire2_dists in

  List.fold_left Stdlib.min Int.max_int combined
