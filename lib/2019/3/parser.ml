open Printf
open InputParser
open Types

let dir_to_delta str =
  let d, v = (str.[0], String.sub str 1 (String.length str - 1)) in
  let d, v = (d, int_of_string v) in

  match d with
  | 'R' -> { x = v; y = 0 }
  | 'L' -> { x = -v; y = 0 }
  | 'U' -> { x = 0; y = v }
  | 'D' -> { x = 0; y = -v }
  | _ -> raise (Invalid_argument (sprintf "Unknown direction %c" d))

let deltas_to_lines deltas =
  let rec helper p d lines =
    match d with
    | [] -> List.rev lines
    | h :: t ->
        let p' = { x = p.x + h.x; y = p.y + h.y } in
        helper p' t ({ p1 = p; p2 = p' } :: lines)
  in

  helper { x = 0; y = 0 } deltas []

let parse_input file_name =
  let lines = read_lines file_name in
  let splits = List.map (fun s -> String.split_on_char ',' s) lines in
  let deltas = List.map (fun wire -> List.map dir_to_delta wire) splits in
  let wires = List.map deltas_to_lines deltas in

  (List.nth wires 0, List.nth wires 1)
