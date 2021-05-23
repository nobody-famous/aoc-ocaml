open Utils

let run file_name =
  let input = Parser.parse_input file_name in
  let seqs = Hashtbl.create 64 in

  let rec loop count =
    if count > 0 then (
      phase seqs input 0;
      loop (count - 1))
  in

  loop 100;

  Array.sub input 0 8
  |> Array.fold_left (fun total item -> (total * 10) + item) 0
