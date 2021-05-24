open AocUtils

type piece = EMPTY | WALL | ENTRANCE | KEY of char | DOOR of char

type pieces = {
  enter : point option;
  empty : (point, char) Hashtbl.t;
  keys : (point, char) Hashtbl.t;
  doors : (point, char) Hashtbl.t;
}

let new_pieces () =
  {
    enter = None;
    empty = Hashtbl.create 64;
    keys = Hashtbl.create 64;
    doors = Hashtbl.create 64;
  }

let piece_to_string = function
  | EMPTY -> "EMPTY"
  | WALL -> "WALL"
  | ENTRANCE -> "ENTRANCE"
  | KEY k -> Printf.sprintf "KEY(%c)" k
  | DOOR d -> Printf.sprintf "DOOR(%c)" d

let char_to_piece ch =
  match ch with
  | '.' -> EMPTY
  | '#' -> WALL
  | '@' -> ENTRANCE
  | 'a' .. 'z' -> KEY ch
  | 'A' .. 'Z' -> DOOR ch
  | _ -> failwith (Printf.sprintf "Invalid input %c" ch)

let is_key = function KEY _ -> true | _ -> false

let is_door = function DOOR _ -> true | _ -> false

let is_empty = function EMPTY -> true | _ -> false
