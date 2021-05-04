let ceil_div num den = if num mod den > 0 then (num / den) + 1 else num / den

let find_min_amount needed required =
  let mul =
    if needed mod required > 0 then (needed / required) + 1
    else needed / required
  in

  mul * required

let add_to_ht ht key value =
  let cur_value = try Hashtbl.find ht key with Not_found -> 0 in
  Hashtbl.replace ht key @@ (value + cur_value)

let sum_chems input =
  let sums = Hashtbl.create 64 in
  let diffs = Hashtbl.create 64 in

  let rec walk (input : (string, Parser.input_chems) Hashtbl.t) name mul =
    let in_chems =
      try Hashtbl.find input name
      with Not_found -> raise @@ Failure (Printf.sprintf "Missing %s" name)
    in

    let top = List.nth in_chems.chems 0 in
    if top.name = "ORE" then add_to_ht sums name mul
    else
      let min_amount = find_min_amount mul in_chems.amount in
      let diff = min_amount - mul in

      if diff > 0 then add_to_ht diffs name diff;

      let extras = try Hashtbl.find diffs name with Not_found -> 0 in

      (* let target = in_chems.amount - extras in *)
      Printf.printf "extras %s %d\n" name extras;
      Hashtbl.replace diffs name 0;

      (* if target > 0 then *)
      List.iter
        (fun (chem : Parser.chemical) ->
          let mul' = max 1 (ceil_div mul in_chems.amount) in

          walk input chem.name (chem.amount * mul'))
        in_chems.chems
  in

  walk input "FUEL" 1;

  Printf.printf "DIFFS\n";
  Hashtbl.iter (fun k v -> Printf.printf "%s %d\n" k v) diffs;

  sums

let find_ores (input : (string, Parser.input_chems) Hashtbl.t) =
  Hashtbl.fold
    (fun name (in_chems : Parser.input_chems) acc ->
      let top : Parser.chemical = List.nth in_chems.chems 0 in
      if top.name = "ORE" then name :: acc else acc)
    input []

let run file_name =
  let input = Parser.parse_input file_name in
  let ores = find_ores input in
  let sums = sum_chems input in

  let answer =
    List.fold_left
      (fun acc name ->
        let amount = Hashtbl.find sums name in
        let required = Hashtbl.find input name in
        let ore_chem = List.nth required.chems 0 in
        let min_amount = find_min_amount amount required.amount in
        let ore_amount = min_amount / required.amount * ore_chem.amount in

        Printf.printf "%s %d %d %d %d %d\n" name amount required.amount
          ore_chem.amount min_amount ore_amount;
        acc + ore_amount)
      0 ores
  in

  Printf.printf "%d\n" answer;
  0
