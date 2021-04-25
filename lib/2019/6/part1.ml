open Parser
open Utils

let count_orbits tree start =
  let counts = Hashtbl.create (Hashtbl.length tree) in
  let rec loop node =
    let count =
      if Hashtbl.mem counts node then Hashtbl.find counts node else 0
    in

    let kids = try Hashtbl.find_all tree node with Not_found -> [] in
    match kids with
    | [] -> ()
    | _ ->
        List.iter
          (fun k ->
            Hashtbl.replace counts k (count + 1);
            loop k)
          kids
  in

  loop start;
  counts

let run file_name =
  let input = parse_input file_name in
  let tree = create_tree input in
  let counts = count_orbits tree "COM" in

  Hashtbl.fold (fun _ count acc -> acc + count) counts 0
