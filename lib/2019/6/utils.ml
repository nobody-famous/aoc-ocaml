let create_tree input =
  let tree = Hashtbl.create (List.length input) in

  List.iter (fun (parent, child) -> Hashtbl.add tree parent child) input;

  tree

