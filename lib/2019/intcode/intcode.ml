open Printf

type machine_state = Halt | Run | NeedInput | HasOutput

type 'a machine = {
  prog : int array;
  payload : 'a;
  memory : (int, int) Hashtbl.t;
  ip : int;
  rel_base : int;
  input : int option;
  output : int option;
  state : machine_state;
  debug : bool;
}

type param_mode = Position | Immediate | Relative

type instruction =
  | Add of (int * int * int)
  | Mul of (int * int * int)
  | Inp of int
  | Out of int
  | JNE of (int * int)
  | JEQ of (int * int)
  | LT of (int * int * int)
  | EQL of (int * int * int)
  | RBO of int
  | HLT

let new_machine payload prog =
  {
    prog;
    payload;
    memory = Hashtbl.create 16;
    ip = 0;
    rel_base = 0;
    input = None;
    output = None;
    state = Run;
    debug = false;
  }

let get_param_mode instr mask =
  let digit = instr / mask mod 10 in
  match digit with
  | 0 -> Position
  | 1 -> Immediate
  | 2 -> Relative
  | _ -> failwith (sprintf "Invalid Mode %d" digit)

let halted mach = mach.state = Halt
let get_prog mach = mach.prog
let set_prog prog mach = { mach with prog }
let set_payload payload mach = { mach with payload }
let get_payload mach = mach.payload
let set_input value mach = { mach with input = Some value }

let get_output mach =
  match mach.output with
  | None -> ({ mach with state = Run }, None)
  | Some v -> ({ mach with output = None; state = Run }, Some v)

let set_state mach state = { mach with state }
let get_state mach = mach.state

let get_addr addr mach =
  if addr < Array.length mach.prog then mach.prog.(addr)
  else try Hashtbl.find mach.memory addr with Not_found -> 0

let set_addr addr value mach =
  if addr < Array.length mach.prog then mach.prog.(addr) <- value
  else Hashtbl.replace mach.memory addr value;
  mach

let set_debug value mach = { mach with debug = value }

let state_to_string s =
  match s with
  | Halt -> "Halt"
  | Run -> "Run"
  | NeedInput -> "NeedInput"
  | HasOutput -> "HasOutput"

let mach_to_string mach =
  sprintf "{IP: %d STATE: %s}" mach.ip @@ state_to_string mach.state

let halt_machine mach = set_state mach Halt

let mode_mask = function
  | 1 -> 100
  | 2 -> 1000
  | 3 -> 10000
  | m -> failwith @@ sprintf "Invalid mode offset %d" m

let read_arg instr offset mach =
  let mode = get_param_mode instr @@ mode_mask offset in
  let value = get_addr (mach.ip + offset) mach in

  match mode with
  | Position -> get_addr value mach
  | Immediate -> value
  | Relative -> get_addr (value + mach.rel_base) mach

let write_arg instr offset mach =
  let value = get_addr (mach.ip + offset) mach in
  let mode = get_param_mode instr @@ mode_mask offset in

  if mode = Relative then value + mach.rel_base else value

let parse_instr mach =
  let op = mach.prog.(mach.ip) in

  let instr =
    match op mod 100 with
    | 1 -> Add (read_arg op 1 mach, read_arg op 2 mach, write_arg op 3 mach)
    | 2 -> Mul (read_arg op 1 mach, read_arg op 2 mach, write_arg op 3 mach)
    | 3 -> Inp (write_arg op 1 mach)
    | 4 -> Out (read_arg op 1 mach)
    | 5 -> JNE (read_arg op 1 mach, read_arg op 2 mach)
    | 6 -> JEQ (read_arg op 1 mach, read_arg op 2 mach)
    | 7 -> LT (read_arg op 1 mach, read_arg op 2 mach, write_arg op 3 mach)
    | 8 -> EQL (read_arg op 1 mach, read_arg op 2 mach, write_arg op 3 mach)
    | 9 -> RBO (read_arg op 1 mach)
    | 99 -> HLT
    | _ -> failwith "parse_instr NOT DONE YET"
  in

  (instr, mach)

let read_input addr mach =
  match mach.input with
  | None -> { mach with state = NeedInput }
  | Some i ->
      let m = set_addr addr i mach in

      if m.debug then printf "%d INP %d -> %d\n" m.ip (get_addr addr m) addr;

      { m with input = None; state = Run; ip = m.ip + 2 }

let write_output value mach =
  if mach.debug then printf "%d OUT %d\n" mach.ip value;

  { mach with output = Some value; state = HasOutput }

let less_than arg1 arg2 addr mach =
  set_addr addr (if arg1 < arg2 then 1 else 0) mach

let equal arg1 arg2 addr mach =
  set_addr addr (if arg1 = arg2 then 1 else 0) mach

let set_ip value mach = { mach with ip = value }
let inc_ip inc mach = set_ip (mach.ip + inc) mach

let exec_instr (instr, mach) =
  match instr with
  | Add (arg1, arg2, addr) -> set_addr addr (arg1 + arg2) @@ inc_ip 4 mach
  | Mul (arg1, arg2, addr) -> set_addr addr (arg1 * arg2) @@ inc_ip 4 mach
  | Inp addr -> read_input addr mach
  | Out arg -> write_output arg @@ inc_ip 2 mach
  | JNE (arg1, arg2) -> if arg1 <> 0 then set_ip arg2 mach else inc_ip 3 mach
  | JEQ (arg1, arg2) -> if arg1 = 0 then set_ip arg2 mach else inc_ip 3 mach
  | LT (arg1, arg2, addr) -> less_than arg1 arg2 addr @@ inc_ip 4 mach
  | EQL (arg1, arg2, addr) -> equal arg1 arg2 addr @@ inc_ip 4 mach
  | RBO arg -> { mach with ip = mach.ip + 2; rel_base = mach.rel_base + arg }
  | HLT -> { mach with state = Halt; ip = mach.ip + 1 }

let dbg_instr (instr, mach) =
  if mach.debug then (
    (match instr with
    | Add (arg1, arg2, addr) -> printf "%d ADD %d,%d %d" mach.ip arg1 arg2 addr
    | Mul (arg1, arg2, addr) -> printf "%d MUL %d,%d %d" mach.ip arg1 arg2 addr
    | Inp addr -> printf "%d INP %d" mach.ip addr
    | Out arg -> printf "%d OUT %d" mach.ip arg
    | _ -> printf "dbg_instr NOT DONE YET");
    (instr, mach))
  else (instr, mach)

let step mach =
  if mach.state = Halt then failwith "step: HaltED";

  mach |> parse_instr |> dbg_instr |> exec_instr

let run_machine in_fn out_fn mach =
  let rec loop mach =
    if get_state mach = Halt then mach
    else
      let mach = step mach in
      match get_state mach with
      | Halt -> mach
      | Run -> loop mach
      | NeedInput -> loop @@ in_fn mach
      | HasOutput -> loop @@ out_fn mach
  in

  loop mach

let parse_input lines =
  match lines with
  | line :: _ ->
      String.split_on_char ',' line |> List.map int_of_string |> Array.of_list
  | [] -> [||]
