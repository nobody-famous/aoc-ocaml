type machine

val new_machine : int array -> machine

val new_machine_io : int array -> (unit -> int) -> (int -> unit) -> machine

val set_addr : machine -> int -> int -> machine

val set_stdin : machine -> (unit -> int) -> machine

val set_stdout : machine -> (int -> unit) -> machine

val set_debug : machine -> bool -> machine

val get_addr : machine -> int -> int

val halted : machine -> bool

val run_prog : machine -> machine

val step : machine -> machine

val parse_input : string -> int array
