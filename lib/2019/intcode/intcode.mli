type machine

val new_machine : int array -> machine

val new_machine_io : int array -> string list -> (int -> unit) -> machine

val set_addr : machine -> int -> int -> machine

val set_debug : machine -> bool -> machine

val get_addr : machine -> int -> int

val run_prog : machine -> machine

val parse_input : string -> int array
