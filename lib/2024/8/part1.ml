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

  (grid, frequencies)

let rec create_pairs pt rest =
  match rest with
  | first :: rest -> (pt, first) :: create_pairs pt rest
  | [] -> []

let rec zip = function
  | first :: rest -> List.concat [ create_pairs first rest; zip rest ]
  | [] -> []

let gen_pairs pts = Hashtbl.to_seq pts |> List.of_seq |> List.map (fun (k, _) -> k) |> zip
let to_tuple x y = (x, y)

let build_pairs (grid, frequencies) =
  let pairs = Hashtbl.create 16 in

  Hashtbl.iter (fun freq pts -> Hashtbl.replace pairs freq @@ gen_pairs pts) frequencies;

  pairs |> Hashtbl.to_seq |> List.of_seq |> List.map (fun (_, v) -> v) |> List.concat |> to_tuple grid

let unzip (a, b) = [ a; b ]
let add_pt (left_row, left_col) (right_row, right_col) = (left_row + right_row, left_col + right_col)
let sub_pt (left_row, left_col) (right_row, right_col) = (left_row - right_row, left_col - right_col)

let pair_antinodes (left, right) =
  let diff = (fst left - fst right, snd left - snd right) in
  [ add_pt left diff; sub_pt right diff ]

let get_antinodes (grid, pairs) = pairs |> List.map pair_antinodes |> List.flatten |> to_tuple grid
let on_grid row col grid = row >= 0 && row < Array.length grid && col >= 0 && col < Array.length grid.(0)

let filter_nodes (grid, nodes) =
  let filtered = Hashtbl.create 16 in
  List.iter (fun (row, col) -> if on_grid row col grid then Hashtbl.replace filtered (row, col) true) nodes;
  filtered |> Hashtbl.to_seq_keys |> List.of_seq

let run lines =
  Aoc.Utils.IntResult
    (Parser.parse_input lines |> find_frequencies |> build_pairs |> get_antinodes |> filter_nodes |> List.length)
