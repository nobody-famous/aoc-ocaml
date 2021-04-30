type slope_side = { pos : bool; neg : bool }

let count_asteroids p1 points =
  let seen = Hashtbl.create 64 in

  let rec loop rem_points =
    match rem_points with
    | [] ->
        Hashtbl.fold
          (fun _ sides acc ->
            let pos = if sides.pos then 1 else 0 in
            let neg = if sides.neg then 1 else 0 in
            acc + pos + neg)
          seen 0
    | p2 :: rest ->
        let x1, y1 = p1 and x2, y2 = p2 in
        let dx = x2 - x1 and dy = y2 - y1 in
        let slope =
          if dx <> 0 then float_of_int dy /. float_of_int dx else Float.infinity
        in

        if not (Hashtbl.mem seen slope) then
          Hashtbl.replace seen slope { pos = false; neg = false };

        let item = Hashtbl.find seen slope in
        if dy > 0 then Hashtbl.replace seen slope { item with pos = true }
        else if dy < 0 then Hashtbl.replace seen slope { item with neg = true }
        else if dx > 0 then Hashtbl.replace seen slope { item with pos = true }
        else if dx < 0 then Hashtbl.replace seen slope { item with neg = true };

        loop rest
  in

  loop points
