open Utils

let run file_name =
  let input = Parser.parse_input file_name in
  let seqs = Hashtbl.create 64 in

  let rec loop arr count =
    if count > 0 then loop (phase seqs arr 0) (count - 1) else arr
  in

  let arr = loop input 100 in
  let pref = Array.sub arr 0 8 in

  Array.fold_left (fun total item -> (total * 10) + item) 0 pref
