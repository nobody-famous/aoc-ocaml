let shift seq = match seq with [] -> [] | _ :: rest -> rest

let seq_to_string seq =
  List.fold_left (fun s n -> Printf.sprintf "%s %d" s n) "" seq

let arr_to_string arr =
  Array.fold_left (fun s n -> Printf.sprintf "%s %d" s n) "" arr

let timer = ref 0

let gen_sequence seqs count target =
  try Hashtbl.find seqs count
  with Not_found ->
    let target = target + 1 in
    let seq = Array.make target 0 in
    let pattern = [| 0; 1; 0; -1 |] in

    let rec loop pat_ndx seq_ndx =
      let rec repeat n ndx =
        if n < count && ndx < Array.length seq then (
          seq.(ndx) <- pattern.(pat_ndx);
          repeat (n + 1) (ndx + 1))
        else ndx
      in

      let seq_ndx = repeat 0 seq_ndx in
      let pat_ndx =
        if pat_ndx + 1 < Array.length pattern then pat_ndx + 1 else 0
      in

      if seq_ndx < Array.length seq then loop pat_ndx seq_ndx
    in

    loop 0 0;

    Hashtbl.replace seqs count seq;

    seq

let build_sum_table input =
  let table = Array.make (Array.length input) 0 in
  let rec loop ndx =
    if ndx < Array.length input then (
      table.(ndx) <- table.(ndx - 1) + input.(ndx);
      loop (ndx + 1))
  in

  table.(0) <- input.(0);
  loop 1;
  table

let apply_seq input seq table count =
  let rec loop total ndx =
    if ndx >= Array.length input then total
    else if seq.(ndx + 1) = 0 then loop total (ndx + count)
    else
      let seq_value = seq.(ndx + 1) in
      let end_ndx = min (ndx + count - 1) (Array.length input - 1) in
      let to_sub = if ndx > 0 then table.(ndx - 1) else 0 in
      let value = (table.(end_ndx) - to_sub) * seq_value in

      loop (total + value) (ndx + count)
  in

  let value = loop 0 (count - 1) in

  abs @@ (value mod 10)

let phase seqs input =
  let output = Array.make (Array.length input) 0 in
  let table = build_sum_table input in

  let rec loop ndx =
    if ndx < Array.length output then (
      let count = ndx + 1 in
      let seq = gen_sequence seqs count (Array.length output) in

      output.(ndx) <- apply_seq input seq table count;
      loop (ndx + 1))
  in

  loop 0;
  output

let run file_name =
  let input = Parser.parse_input file_name in
  let seqs = Hashtbl.create 64 in

  let rec loop arr count =
    if count > 0 then loop (phase seqs arr) (count - 1) else arr
  in

  let arr = loop input 100 in
  let pref = Array.sub arr 0 8 in

  Array.fold_left (fun total item -> (total * 10) + item) 0 pref
