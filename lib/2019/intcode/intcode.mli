type 'a machine
type machine_state = Halt | Run | NeedInput | HasOutput

val new_machine : 'a -> int array -> 'a machine
val get_prog : 'a machine -> int array
val set_prog : int array -> 'a machine -> 'a machine
val set_payload : 'a -> 'a machine -> 'a machine
val get_payload : 'a machine -> 'a
val set_input : int -> 'a machine -> 'a machine
val get_output : 'a machine -> 'a machine * int option
val set_addr : int -> int -> 'a machine -> 'a machine
val set_debug : bool -> 'a machine -> 'a machine
val get_addr : int -> 'a machine -> int
val set_state : 'a machine -> machine_state -> 'a machine
val get_state : 'a machine -> machine_state
val halted : 'a machine -> bool
val mach_to_string : 'a machine -> string
val state_to_string : machine_state -> string
val parse_input : string list -> int array
val halt_machine : 'a machine -> 'a machine
val step : 'a machine -> 'a machine

val run_machine :
  ('a machine -> 'a machine) ->
  ('a machine -> 'a machine) ->
  'a machine ->
  'a machine
