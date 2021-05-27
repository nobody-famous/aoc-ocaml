open AocUtils

type piece = EMPTY | WALL | ENTRANCE | KEY of char | DOOR of char

type pieces = {
  enter : point;
  empty : (point, char) Hashtbl.t;
  keys : (point, char) Hashtbl.t;
  doors : (point, char) Hashtbl.t;
}

let door_to_key door = Char.lowercase_ascii door

let key_mask = function
  | 'a' -> 0x1
  | 'b' -> 0x2
  | 'c' -> 0x4
  | 'd' -> 0x8
  | 'e' -> 0x10
  | 'f' -> 0x20
  | 'g' -> 0x40
  | 'h' -> 0x80
  | 'i' -> 0x100
  | 'j' -> 0x200
  | 'k' -> 0x400
  | 'l' -> 0x800
  | 'm' -> 0x1000
  | 'n' -> 0x2000
  | 'o' -> 0x4000
  | 'p' -> 0x8000
  | 'q' -> 0x10000
  | 'r' -> 0x20000
  | 's' -> 0x40000
  | 't' -> 0x80000
  | 'u' -> 0x100000
  | 'v' -> 0x200000
  | 'w' -> 0x400000
  | 'x' -> 0x800000
  | 'y' -> 0x1000000
  | 'z' -> 0x2000000
  | k -> failwith @@ Printf.sprintf "key_mask: Invalid key %c" k

let new_pieces () =
  {
    enter = { x = 0; y = 0 };
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
