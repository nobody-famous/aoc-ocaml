type piece =
  | NEW_LINE
  | SCAFFOLD
  | SPACE
  | ROBOT_UP
  | ROBOT_DOWN
  | ROBOT_RIGHT
  | ROBOT_LEFT

let piece_to_char = function
  | NEW_LINE -> '\n'
  | SCAFFOLD -> '#'
  | SPACE -> '.'
  | ROBOT_UP -> '^'
  | ROBOT_DOWN -> 'v'
  | ROBOT_RIGHT -> '>'
  | ROBOT_LEFT -> '<'

let piece_to_string = function
  | NEW_LINE -> "NEW_LINE"
  | SCAFFOLD -> "SCAFFOLD"
  | SPACE -> "SPACE"
  | ROBOT_UP -> "ROBOT_UP"
  | ROBOT_DOWN -> "ROBOT_DOWN"
  | ROBOT_RIGHT -> "ROBOT_RIGHT"
  | ROBOT_LEFT -> "ROBOT_LEFT"

let char_to_piece = function
  | '\n' -> NEW_LINE
  | '#' -> SCAFFOLD
  | '.' -> SPACE
  | '^' -> ROBOT_UP
  | '>' -> ROBOT_RIGHT
  | 'v' -> ROBOT_DOWN
  | '<' -> ROBOT_LEFT
  | ch -> raise @@ Failure (Printf.sprintf "Invalid piece %c" ch)

type point = { row : int; col : int }

type scaffold_state = { loc : point; board : (point, piece) Hashtbl.t }

let new_state = { loc = { row = 0; col = 0 }; board = Hashtbl.create 64 }

type board_bounds = {
  low_row : int;
  high_row : int;
  low_col : int;
  high_col : int;
}

let new_bounds =
  {
    low_row = max_int;
    high_row = min_int;
    low_col = max_int;
    high_col = min_int;
  }

let get_board_bounds board =
  Hashtbl.fold
    (fun pt _ acc ->
      {
        low_row = min acc.low_row pt.row;
        high_row = max acc.high_row pt.row;
        low_col = min acc.low_col pt.col;
        high_col = max acc.high_col pt.col;
      })
    board new_bounds

let is_scaffold = function
  | SCAFFOLD | ROBOT_UP | ROBOT_DOWN | ROBOT_RIGHT | ROBOT_LEFT -> true
  | _ -> false

  let is_cross pt board =
    let get_piece pt = try Hashtbl.find board pt with Not_found -> SPACE in
  
    let center = get_piece pt in
    let north = get_piece { pt with row = pt.row - 1 } in
    let south = get_piece { pt with row = pt.row + 1 } in
    let east = get_piece { pt with col = pt.col + 1 } in
    let west = get_piece { pt with col = pt.col - 1 } in
  
    is_scaffold center && is_scaffold north && is_scaffold south
    && is_scaffold east && is_scaffold west
  
let print_board board =
  let bounds = get_board_bounds board in

  let rec row_loop row =
    let rec col_loop col =
      if col < bounds.high_col then (
        let piece =
          try Hashtbl.find board { row; col } with Not_found -> SPACE
        in

        if is_cross { row; col } board then Printf.printf "O"
        else Printf.printf "%c" @@ piece_to_char piece;

        col_loop (col + 1))
    in

    col_loop bounds.low_col;
    Printf.printf "\n";

    if row < bounds.high_row then row_loop (row + 1)
  in

  row_loop bounds.low_row

  let handle_output mach =
    let state = Intcode.get_payload mach in
    let m, out = Intcode.get_output mach in
  
    match out with
    | None -> raise @@ Failure "Expected output, but had none"
    | Some v ->
        let ht = state.board in
        let piece = char_of_int v |> char_to_piece in
        let new_loc =
          if piece = NEW_LINE then { row = state.loc.row + 1; col = 0 }
          else { row = state.loc.row; col = state.loc.col + 1 }
        in
  
        if is_scaffold piece then Hashtbl.replace ht state.loc piece;
  
        Intcode.set_payload { state with loc = new_loc } m
  