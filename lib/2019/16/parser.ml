let parse_input file_name =
  let lines = InputParser.read_lines file_name in
  let line =
    match lines with [] -> raise @@ Failure "No input" | first :: _ -> first
  in
  let out = Array.make (String.length line) 0 in

  let _ =
    Seq.fold_left
      (fun ndx ch ->
        out.(ndx) <- Char.code ch - Char.code '0';
        ndx + 1)
      0 (String.to_seq line)
  in

  out
