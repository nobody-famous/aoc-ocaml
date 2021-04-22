open Printf
open InputParser

type point = { x : int; y : int }

type line = { p1 : point; p2 : point }

let dir_to_delta str =
  let d, v = (str.[0], String.sub str 1 (String.length str - 1)) in
  let d, v = (d, int_of_string v) in

  match d with
  | 'R' -> { x = v; y = 0 }
  | 'L' -> { x = -v; y = 0 }
  | 'U' -> { x = 0; y = v }
  | 'D' -> { x = 0; y = -v }
  | _ -> raise (Invalid_argument (sprintf "Unknown direction %c" d))

let parse_input file_name =
  let lines = read_lines file_name in
  let splits = List.map (fun s -> String.split_on_char ',' s) lines in

   List.map (fun wire -> List.map dir_to_delta wire) splits 
