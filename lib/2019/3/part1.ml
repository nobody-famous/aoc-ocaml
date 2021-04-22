open Printf
open Parser

let run file_name =
  let deltas = parse_input file_name in

  List.iter
    (fun line ->
      printf "LINE\n";
      List.iter (fun p -> printf "ITEM %d %d\n" p.x p.y) line)
    deltas
