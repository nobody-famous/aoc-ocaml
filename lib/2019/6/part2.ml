open Parser
open Utils

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

let remove_common path1 path2 =
  let rec loop p1 p2 =
    match (p1, p2) with
    | first :: rest, first' :: rest' ->
        if first = first' then loop rest rest' else (p1, p2)
    | _, _ -> (path1, path2)
  in

  loop path1 path2

let run file_name =
  let tree = parse_input file_name |> create_tree in
  let you_path = find_path tree "YOU" and san_path = find_path tree "SAN" in

  let you_path, san_path =
    match (you_path, san_path) with
    | Some y, Some s -> remove_common y s
    | _, _ -> ([], [])
  in

  List.length you_path - 1 + (List.length san_path - 1)
