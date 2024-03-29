open Printf
open Types

let dir_to_delta str =
  let d, v =
    (str.[0], int_of_string @@ String.sub str 1 (String.length str - 1))
  in

  match d with
  | 'R' -> { x = v; y = 0 }
  | 'L' -> { x = -v; y = 0 }
  | 'U' -> { x = 0; y = v }
  | 'D' -> { x = 0; y = -v }
  | _ -> raise @@ Invalid_argument (sprintf "Unknown direction %c" d)

let deltas_to_lines deltas =
  let rec loop p d lines =
    match d with
    | [] -> List.rev lines
    | h :: t ->
        let p' = { x = p.x + h.x; y = p.y + h.y } in
        loop p' t @@ ({ p1 = p; p2 = p' } :: lines)
  in

  loop { x = 0; y = 0 } deltas []

let parse_input lines =
  let wires =
    lines
    |> List.map (fun s -> String.split_on_char ',' s)
    |> List.map (fun wire -> List.map dir_to_delta wire)
    |> List.map deltas_to_lines
  in

  (List.nth wires 0, List.nth wires 1)
