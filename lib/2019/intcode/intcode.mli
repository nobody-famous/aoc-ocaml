type machine

type machine_state = HALT | RUN | INPUT | OUTPUT

val new_machine : int array -> machine

val set_input : machine -> int -> machine

val get_output : machine -> machine * int option

val set_addr : machine -> int -> int -> machine

val set_debug : machine -> bool -> machine

val get_addr : machine -> int -> int

val get_state : machine -> machine_state

val halted : machine -> bool

val step : machine -> machine

val mach_to_string : machine -> string

val state_to_string : machine_state -> string

val parse_input : string -> int array
