type position = { row : int; col : int }
type 'a edge = { target : position; weight : 'a }
type 'a node = { edges : 'a edge list }
type 'a graph = (position, 'a node) Hashtbl.t
