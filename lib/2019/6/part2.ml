open Parser
open Utils

let print_path path =
  match path with
  | Some p ->
      List.iter (fun n -> Printf.printf "%s->" n) p;
      print_endline ""
  | None -> ()

let find_path tree target =
  let rec loop path node =
    if node = target then Some (List.rev path)
    else
      let kids = try Hashtbl.find_all tree node with Not_found -> [] in
      let rec kids_loop k =
        match k with
        | [] -> None
        | first :: rest -> (
            match loop (first :: path) first with
            | Some p -> Some p
            | None -> kids_loop rest)
      in

      kids_loop kids
  in

  loop [] "COM"

let run file_name =
  let input = parse_input file_name in
  let tree = create_tree input in
  let you_path = find_path tree "YOU" and san_path = find_path tree "SAN" in

  print_path you_path;
  print_path san_path
