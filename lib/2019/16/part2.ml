open Utils

let mult_input times input =
  let length = Array.length input * times in
  let out = Array.make length 0 in

  let rec loop ndx =
    if ndx < Array.length out then (
      Array.blit input 0 out ndx (Array.length input);
      loop (ndx + Array.length input))
  in

  loop 0;
  out

let arr_to_num arr =
  Array.fold_left (fun total item -> (total * 10) + item) 0 arr

let run file_name =
  let input = Parser.parse_input file_name |> mult_input 10000 in
  let seqs = Hashtbl.create 64 in

  let offset = arr_to_num @@ Array.sub input 0 7 in

  let rec loop count =
    if count > 0 then (
      phase seqs input offset;
      loop (count - 1))
  in

  loop 100;

  let pref = Array.sub input offset 8 in

  Array.fold_left (fun total item -> (total * 10) + item) 0 pref
