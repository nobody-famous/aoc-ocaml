type 'a machine

type machine_state = HALT | RUN | INPUT | OUTPUT

val new_machine : 'a -> int array -> 'a machine

val set_payload : 'a machine -> 'a -> 'a machine

val get_payload : 'a machine -> 'a

val set_input : 'a machine -> int -> 'a machine

val get_output : 'a machine -> 'a machine * int option

val set_addr : 'a machine -> int -> int -> 'a machine

val set_debug : 'a machine -> bool -> 'a machine

val get_addr : 'a machine -> int -> int

val set_state : 'a machine -> machine_state -> 'a machine

val get_state : 'a machine -> machine_state

val halted : 'a machine -> bool

val step : 'a machine -> 'a machine

val mach_to_string : 'a machine -> string

val state_to_string : machine_state -> string

val parse_input : string -> int array

val run_machine :
  ('a machine -> 'a machine) ->
  ('a machine -> 'a machine) ->
  'a machine ->
  'a machine
