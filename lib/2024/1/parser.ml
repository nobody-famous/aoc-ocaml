type inputItem = { left : int; right : int }

let group_to_item grp = { left = int_of_string (Re.Group.get grp 1); right = int_of_string (Re.Group.get grp 2) }
let parse_line line = "(\\d+)\\s+(\\d+)" |> Re.Perl.compile_pat |> (fun re -> Re.exec re line) |> group_to_item
let rev_lists (left, right) = (List.rev left, List.rev right)

let unzip items =
  items |> List.fold_left (fun (left, right) item -> (item.left :: left, item.right :: right)) ([], []) |> rev_lists

let parse_input lines = lines |> List.map parse_line |> unzip
