let arr_to_string arr =
  Array.fold_left (fun s n -> Printf.sprintf "%s %d" s n) "" arr

let gen_sequence _ _ _ = [| 0; 1; 0; -1 |]

let timer = ref 0

let build_sum_table input =
  let table = Array.make (Array.length input) 0 in
  let rec loop ndx =
    if ndx < Array.length input then (
      table.(ndx) <- table.(ndx - 1) + input.(ndx);
      loop (ndx + 1))
  in

  table.(0) <- input.(0);

  let start = int_of_float (Unix.gettimeofday () *. 1000.0) in
  loop 1;
  let diff = int_of_float (Unix.gettimeofday () *. 1000.0) - start in
  timer := !timer + diff;
  table

let inc_seq_ndx seq ndx = if ndx + 1 >= Array.length seq then 0 else ndx + 1

let apply_seq input seq table count =
  let rec loop total seq_ndx ndx =
    if ndx >= Array.length input then total
    else if seq.(seq_ndx) = 0 then
      loop total (inc_seq_ndx seq seq_ndx) (ndx + count)
    else
      let seq_value = seq.(seq_ndx) in
      let end_ndx = min (ndx + count - 1) (Array.length input - 1) in
      let to_sub = if ndx > 0 then table.(ndx - 1) else 0 in
      let value = (table.(end_ndx) - to_sub) * seq_value in

      loop (total + value) (inc_seq_ndx seq seq_ndx) (ndx + count)
  in

  let value = loop 0 1 (count - 1) in

  abs @@ (value mod 10)

let phase seqs input offset =
  let output = Array.make (Array.length input) 0 in
  let table = build_sum_table input in

  let rec loop ndx =
    if ndx < Array.length output then (
      let count = ndx + 1 in
      let seq = gen_sequence seqs count (Array.length output) in

      output.(ndx) <- apply_seq input seq table count;
      loop (ndx + 1))
  in

  loop offset;
  output
