open Printf
open InputParser

let parse_input file_name =
  let lines = read_lines file_name in
  let splits = List.map (fun s -> String.split_on_char ',' s) lines in

  List.iter
    (fun line ->
      printf "LINE\n";
      List.iter (fun item -> printf "ITEM %s\n" item) line)
    splits
