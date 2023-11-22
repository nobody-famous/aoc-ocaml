let parse_input lines =
  let line = lines |> InputParser.get_first_line in
  let out = Array.make (String.length line) 0 in

  let _ =
    Seq.fold_left
      (fun ndx ch ->
        out.(ndx) <- Char.code ch - Char.code '0';
        ndx + 1)
      0 (String.to_seq line)
  in

  out
