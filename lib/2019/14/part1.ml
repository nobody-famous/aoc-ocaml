let ceil_div num den = (num + (den - 1)) / den

let find_min_amount needed required =
  let mul = ceil_div needed required in

  mul * required

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

  let rec loop is_done =
    if is_done then ()
    else
      Hashtbl.iter
        (fun key value ->
          if value = 0 then
            let n = Hashtbl.find req key in
            match key with
            | "ORE" ->
                Printf.printf "%d\n" n;
                loop true
            | _ ->
                (let entry = Hashtbl.find input key in
                 let amt = ceil_div n entry.amount in

                 List.iter
                   (fun (chem : Parser.chemical) ->
                     add_to_ht req chem.name (amt * chem.amount);
                     add_to_ht seen chem.name (-1))
                   entry.chems);
                Hashtbl.remove seen key;
                loop false)
        seen
  in

  loop false

let run file_name =
  let input = Parser.parse_input file_name in
  let _ = calc_ore input in

  let answer = 0 in

  Printf.printf "%d\n" answer;

  answer
