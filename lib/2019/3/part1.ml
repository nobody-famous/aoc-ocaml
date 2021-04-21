open Printf
open Parser

let run file_name =
  parse_input file_name;
  printf "Day 3 Part 1 %s\n" file_name
