let group_lines lines =
  List.fold_left
    (fun acc line ->
      match acc with
      | hd :: rest -> (
          match line with
          | "" -> [] :: hd :: rest
          | _ -> (line :: hd) :: rest)
      | _ -> acc)
    [ [] ] lines
  |> List.map (fun grp -> List.rev grp)
  |> List.rev

let parse_input lines =
  let groups = group_lines lines in

  Printf.printf "***** GROUPS %d\n" @@ List.length groups;
  List.iter
    (fun grp -> List.iter (fun line -> Printf.printf "%s\n" line) grp)
    groups;
  0
