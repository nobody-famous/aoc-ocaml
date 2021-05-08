let shift seq = match seq with [] -> [] | _ :: rest -> rest

let seq_to_string seq =
  List.fold_left (fun s n -> Printf.sprintf "%s %d" s n) "" seq

let arr_to_string arr =
  Array.fold_left (fun s n -> Printf.sprintf "%s %d" s n) "" arr

let timer = ref 0

let gen_sequence seqs count target =
  try Hashtbl.find seqs count
  with Not_found ->
    let rec grow seq to_add =
      if List.length seq > target then seq else grow (seq @ to_add) to_add
    in

    let rec loop seq rem =
      let rec repeat seq n item =
        if n > 0 then repeat (item :: seq) (n - 1) item else seq
      in

      match rem with
      | [] -> List.rev seq |> grow (shift @@ List.rev seq)
      | first :: rest -> loop (repeat seq count first) rest
    in

    let seq = loop [] [ 0; 1; 0; -1 ] in
    Hashtbl.replace seqs count seq;

    seq

let apply_seq input seq =
  let _, value =
    Array.fold_left
      (fun acc item ->
        let s, v = acc in

        match s with
        | [] -> raise @@ Failure "Ran out of sequence"
        | first :: rest ->
            let rem = match rest with [] -> seq | _ -> rest in
            let total = v + (item * first) in

            (rem, total))
      (seq, 0) input
  in

  abs @@ (value mod 10)

let phase seqs input =
  let output = Array.make (Array.length input) 0 in

  let rec loop ndx =
    if ndx < Array.length output then (
      let seq = gen_sequence seqs (ndx + 1) (Array.length output) in

      output.(ndx) <- apply_seq input seq;
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

  Printf.printf "timer %d\n" !timer;

  Array.fold_left (fun total item -> (total * 10) + item) 0 pref
