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

let rec create_pairs pt rest =
  match rest with
  | first :: rest -> (pt, first) :: create_pairs pt rest
  | [] -> []

let rec zip = function
  | first :: rest -> List.concat [ create_pairs first rest; zip rest ]
  | [] -> []

(* let gen_pairs (pts : (int * int, bool) Hashtbl.t) = *)
let gen_pairs pts = Hashtbl.to_seq pts |> List.of_seq |> List.map (fun (k, _) -> k) |> zip

let build_pairs frequencies =
  let pairs = Hashtbl.create 16 in

  Hashtbl.iter (fun freq pts -> Hashtbl.replace pairs freq @@ gen_pairs pts) frequencies;

  pairs |> Hashtbl.to_seq |> List.of_seq |> List.map (fun (_, v) -> v) |> List.concat

let unzip (a, b) = [ a; b ]

let pair_antinodes (left, right) =
  let row_diff = fst left - fst right in
  let col_diff = snd left - snd right in
  [ (left, right); (left, right) ]

let get_antinodes pairs = pairs |> List.map pair_antinodes |> List.flatten

let run lines =
  let tmp = Parser.parse_input lines |> find_frequencies |> build_pairs |> get_antinodes in
  Printf.printf "***** TMP %d\n" @@ List.length tmp;
  Aoc.Utils.IntResult 0
