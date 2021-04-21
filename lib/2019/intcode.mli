type machine

val new_machine : int array -> machine

val set_addr : machine -> int -> int -> machine

val get_addr : machine -> int -> int

val run_prog : machine -> machine
