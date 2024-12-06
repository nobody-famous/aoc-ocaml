type inputItem = { left : int; right : int }

let parse_line line =
  let matcher = Re.Perl.compile_pat "(\\d+)\\s+(\\d+)" in
  let group = Re.exec matcher line in

  Printf.printf "***** MATCHED %s, %s, %s\n" (Re.Group.get group 0) (Re.Group.get group 1) (Re.Group.get group 2);
  ()

let parse_input lines = lines |> List.iter parse_line
