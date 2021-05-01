type point = { x : int; y : int }

type slope_side = { pos : point list; neg : point list }

let count_visible slope_map =
  Hashtbl.fold
    (fun _ sides acc ->
      let pos = if List.length sides.pos > 0 then 1 else 0 in
      let neg = if List.length sides.neg > 0 then 1 else 0 in
      acc + pos + neg)
    slope_map 0

let build_map p1 points =
  let seen = Hashtbl.create 64 in

  let rec loop rem_points =
    match rem_points with
    | [] -> seen
    | p2 :: rest ->
        let dx = p2.x - p1.x and dy = p2.y - p1.y in
        let slope =
          if dx <> 0 then float_of_int dy /. float_of_int dx else Float.infinity
        in

        if p1.x = 3 && p1.y = 4 && p2.x = 0 && p2.y = 2 then
          Printf.printf "HERE %d %d %f\n" dx dy slope;

          if not (Hashtbl.mem seen slope) then
          Hashtbl.replace seen slope { pos = []; neg = [] };

        let item = Hashtbl.find seen slope in
        if dy > 0 then
          Hashtbl.replace seen slope { item with pos = p2 :: item.pos }
        else if dy < 0 then
          Hashtbl.replace seen slope { item with neg = p2 :: item.neg }
        else if dx > 0 then
          Hashtbl.replace seen slope { item with pos = p2 :: item.pos }
        else if dx < 0 then
          Hashtbl.replace seen slope { item with neg = p2 :: item.neg };

        loop rest
  in

  loop points
