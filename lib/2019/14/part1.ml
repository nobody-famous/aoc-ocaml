let ceil_div num den = (num + (den - 1)) / den

let add_to_ht ht key value =
  let cur_value = try Hashtbl.find ht key with Not_found -> 0 in
  Hashtbl.replace ht key @@ (value + cur_value)

let calc_ore (input : (string, Parser.reaction) Hashtbl.t) =
  let seen = Hashtbl.create 64 in
  let req = Hashtbl.create 64 in

  Hashtbl.iter
    (fun _ (v : Parser.reaction) ->
      List.iter
        (fun (chem : Parser.chemical) -> add_to_ht seen chem.name 1)
        v.chems)
    input;

  Hashtbl.replace seen "FUEL" 0;
  Hashtbl.replace req "FUEL" 1;

  let rec loop () =
    let n =
      Hashtbl.fold
        (fun key value acc ->
          if value <> 0 then acc
          else
            let n = Hashtbl.find req key in

            match key with
            | "ORE" -> Some n
            | _ ->
                (let entry = Hashtbl.find input key in
                 let amt = ceil_div n entry.amount in

                 List.iter
                   (fun (chem : Parser.chemical) ->
                     add_to_ht req chem.name (amt * chem.amount);
                     add_to_ht seen chem.name (-1))
                   entry.chems);
                Hashtbl.remove seen key;
                acc)
        seen None
    in

    match n with None -> loop () | Some value -> value
  in

  loop ()

let run file_name =
  let input = Parser.parse_input file_name in
  let answer = calc_ore input in

  answer
