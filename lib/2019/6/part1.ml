open Parser

let create_parents input =
  let parents = Hashtbl.create (List.length input) in

  List.iter (fun (p, c) -> Hashtbl.add parents c p) input;

  parents

let run file_name =
  let input = parse_input file_name in
  let parents = create_parents input in

  Hashtbl.iter (fun c p -> Printf.printf "%s -> %s\n" c p) parents;
  0
