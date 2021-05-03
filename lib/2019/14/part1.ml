let run file_name =
  let input = Parser.parse_input file_name in
  let fuel =
    try Hashtbl.find input "FUEL"
    with Not_found -> raise @@ Failure "FUEL NOT FOUND"
  in

  Printf.printf "%d %d\n" fuel.amount @@ List.length fuel.chems;
  0
