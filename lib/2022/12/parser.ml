module G = Aoc.Graph

type cell = { pos : Utils.position; ch : char }

let to_cells row line = line |> String.to_seq |> List.of_seq |> List.mapi (fun col ch -> { pos = { row; col }; ch })

let to_map =
  List.fold_left
    (fun ht c ->
      Hashtbl.replace ht c.pos c.ch;
      ht)
    (Hashtbl.create 64)

let parse_input lines = List.mapi to_cells lines |> List.flatten |> to_map
