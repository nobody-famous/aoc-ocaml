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

  Printf.printf "input %d\n" @@ Array.length input;
  let offset = arr_to_num @@ Array.sub input 0 7 in
  Printf.printf "offset %d\n" offset;

  let rec loop arr count =
    if count > 0 then loop (phase seqs arr offset) (count - 1) else arr
  in

  let start = int_of_float (Unix.gettimeofday () *. 1000.0) in
  let arr = loop input 10 in
  Printf.printf "%d %d\n" !timer
  @@ (int_of_float (Unix.gettimeofday () *. 1000.0) - start);

  let pref = Array.sub arr 0 8 in

  let answer = Array.fold_left (fun total item -> (total * 10) + item) 0 pref in
  Printf.printf "%d\n" answer;
  answer
