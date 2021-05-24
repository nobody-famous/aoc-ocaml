open AocUtils

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
  | ch -> failwith (Printf.sprintf "Invalid piece %c" ch)

let is_piece v =
  let ch = char_of_int v in
  match ch with '\n' | '#' | '.' | '^' | '>' | 'v' | '<' -> true | _ -> false

type scaffold_state = {
  loc : point;
  robot : point;
  board : (point, piece) Hashtbl.t;
}

let new_state =
  {
    loc = { x = 0; y = 0 };
    robot = { x = 0; y = 0 };
    board = Hashtbl.create 64;
  }

let is_scaffold = function SCAFFOLD -> true | _ -> false

let is_robot = function
  | ROBOT_UP | ROBOT_DOWN | ROBOT_RIGHT | ROBOT_LEFT -> true
  | _ -> false

let is_cross pt board =
  let get_piece pt = try Hashtbl.find board pt with Not_found -> SPACE in

  let center = get_piece pt in
  let north = get_piece { pt with x = pt.x - 1 } in
  let south = get_piece { pt with x = pt.x + 1 } in
  let east = get_piece { pt with y = pt.y + 1 } in
  let west = get_piece { pt with y = pt.y - 1 } in

  (is_robot center || is_scaffold center)
  && (is_robot north || is_scaffold north)
  && (is_robot south || is_scaffold south)
  && (is_robot east || is_scaffold east)
  && (is_robot west || is_scaffold west)

let print_board board =
  let bounds = get_board_bounds board in

  let rec x_loop x =
    let rec y_loop y =
      if y <= bounds.high_y then (
        let piece = try Hashtbl.find board { x; y } with Not_found -> SPACE in

        if is_cross { x; y } board then Printf.printf "O"
        else Printf.printf "%c" @@ piece_to_char piece;

        y_loop (y + 1))
    in

    y_loop bounds.low_y;
    Printf.printf "\n";

    if x < bounds.high_x then x_loop (x + 1)
  in

  x_loop bounds.low_x

let handle_output mach =
  let state = Intcode.get_payload mach in
  let m, out = Intcode.get_output mach in

  match out with
  | None -> failwith "Expected output, but had none"
  | Some v ->
      let ht = state.board in

      if is_piece v then (
        let piece = char_of_int v |> char_to_piece in
        let new_loc =
          if piece = NEW_LINE then { x = state.loc.x + 1; y = 0 }
          else { x = state.loc.x; y = state.loc.y + 1 }
        in
        let state =
          if is_robot piece then { state with robot = state.loc } else state
        in

        if is_robot piece || is_scaffold piece then
          Hashtbl.replace ht state.loc piece;

        Intcode.set_payload { state with loc = new_loc } m)
      else m
