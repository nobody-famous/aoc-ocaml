let ceil_div num den = (num + (den - 1)) / den

let inc_ht_entry ht key value =
  let cur_value = try Hashtbl.find ht key with Not_found -> 0 in
  Hashtbl.replace ht key @@ (value + cur_value)

let count_parents input num_parents =
  Hashtbl.iter
    (fun _ (v : Parser.reaction) ->
      List.iter
        (fun (chem : Parser.chemical) -> inc_ht_entry num_parents chem.name 1)
        v.chems)
    input

let calc_ore input fuel_target =
  let parent_counts = Hashtbl.create 64 in
  let req = Hashtbl.create 64 in

  count_parents input parent_counts;

  Hashtbl.replace parent_counts "FUEL" 0;
  Hashtbl.replace req "FUEL" fuel_target;

  let rec loop () =
    let n =
      Hashtbl.fold
        (fun name parent_count acc ->
          if parent_count <> 0 then acc
          else
            let n = Hashtbl.find req name in

            match name with
            | "ORE" -> Some n
            | _ ->
                (let entry = Hashtbl.find input name in
                 let amt = ceil_div n entry.amount in

                 List.iter
                   (fun (chem : Parser.chemical) ->
                     inc_ht_entry req chem.name (amt * chem.amount);
                     inc_ht_entry parent_counts chem.name (-1))
                   entry.chems);
                Hashtbl.remove parent_counts name;
                acc)
        parent_counts None
    in

    match n with None -> loop () | Some value -> value
  in

  loop ()
