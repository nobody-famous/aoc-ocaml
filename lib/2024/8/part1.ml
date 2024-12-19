let add_frequency freq pt map =
  match Hashtbl.find_opt map freq with
  | Some set -> Hashtbl.replace set pt true
  | None ->
      let set = Hashtbl.create 16 in
      Hashtbl.replace set pt true;
      Hashtbl.replace map freq set

let find_frequencies grid =
  let frequencies = Hashtbl.create 16 in

  for row = 0 to Array.length grid - 1 do
    for col = 0 to Array.length grid.(row) - 1 do
      let ch = grid.(row).(col) in
      if ch != '.' then
        add_frequency ch (row, col) frequencies
    done
  done;

  frequencies

let gen_pairs pts = pts

let build_pairs frequencies =
  let pairs = Hashtbl.create 16 in

  Hashtbl.iter (fun freq pts -> Hashtbl.replace pairs freq @@ gen_pairs pts) frequencies;

  frequencies

let run lines =
  let _ = Parser.parse_input lines |> find_frequencies |> build_pairs in
  Aoc.Utils.IntResult 0
