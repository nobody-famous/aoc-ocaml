open Printf

type machine_state = HALT | RUN | INPUT | OUTPUT

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

type op = { code : int; modes : param_mode array }

let new_machine payload prog =
  {
    prog;
    payload;
    memory = Hashtbl.create 16;
    ip = 0;
    rel_base = 0;
    input = None;
    output = None;
    state = RUN;
    debug = false;
  }

let get_param_mode instr mask =
  let digit = instr / mask mod 10 in
  match digit with
  | 0 -> Position
  | 1 -> Immediate
  | 2 -> Relative
  | _ -> raise @@ Failure (Printf.sprintf "Invalid Mode %d" digit)

let int_to_op instr =
  {
    code = instr mod 100;
    modes =
      [|
        get_param_mode instr 100;
        get_param_mode instr 1000;
        get_param_mode instr 10000;
      |];
  }

let halted m = m.state = HALT

let get_prog m = m.prog

let set_prog prog m = { m with prog }

let set_payload p m = { m with payload = p }

let get_payload m = m.payload

let set_input value m = { m with input = Some value }

let get_output m =
  match m.output with
  | None -> ({ m with state = RUN }, None)
  | Some v -> ({ m with output = None; state = RUN }, Some v)

let set_state m s = { m with state = s }

let get_state m = m.state

let get_addr addr m =
  if addr < Array.length m.prog then m.prog.(addr)
  else try Hashtbl.find m.memory addr with Not_found -> 0

let set_addr addr value m =
  if addr < Array.length m.prog then m.prog.(addr) <- value
  else Hashtbl.replace m.memory addr value;
  m

let set_debug m value = { m with debug = value }

let param_value m op offset =
  let v = get_addr (m.ip + offset) m in
  let value =
    match op.modes.(offset - 1) with
    | Position -> get_addr v m
    | Immediate -> v
    | Relative -> get_addr (v + m.rel_base) m
  in

  value

let write_addr m op offset =
  let addr = get_addr (m.ip + offset) m in
  if op.modes.(offset - 1) = Relative then addr + m.rel_base else addr

let math_op label m op fn =
  let param_1 = param_value m op 1
  and param_2 = param_value m op 2
  and addr = write_addr m op 3 in
  let value = fn param_1 param_2 in

  if m.debug then
    printf "%d %s %d %d = (%d) -> %d\n" m.ip label param_1 param_2 value addr;

  let m = set_addr addr value m in
  { m with ip = m.ip + 4 }

let op_code_1 m op = math_op "ADD" m op (fun a b -> a + b)

let op_code_2 m op = math_op "MUL" m op (fun a b -> a * b)

let op_code_3 m op =
  let addr = write_addr m op 1 in

  match m.input with
  | None -> { m with state = INPUT }
  | Some i ->
      let m = set_addr addr i m in

      if m.debug then printf "%d INP %d -> %d\n" m.ip (get_addr addr m) addr;

      { m with input = None; state = RUN; ip = m.ip + 2 }

let op_code_4 m op =
  let addr = get_addr (m.ip + 1) m in
  let value = param_value m op 1 in

  if m.debug then printf "%d OUT %d (%d)\n" m.ip addr value;

  { m with output = Some value; state = OUTPUT; ip = m.ip + 2 }

let jmp m op fn =
  let param_1 = param_value m op 1 and param_2 = param_value m op 2 in

  if m.debug then printf "%d JMP %d %d\n" m.ip param_1 param_2;

  if fn param_1 then { m with ip = param_2 } else { m with ip = m.ip + 3 }

let op_code_5 m op = jmp m op (fun n -> n <> 0)

let op_code_6 m op = jmp m op (fun n -> n = 0)

let op_code_7 m op =
  let param_1 = param_value m op 1
  and param_2 = param_value m op 2
  and addr = write_addr m op 3 in

  let value = if param_1 < param_2 then 1 else 0 in

  if m.debug then
    printf "%d LT %d %d (%d) -> %d\n" m.ip param_1 param_2 value addr;

  let m = set_addr addr value m in
  { m with ip = m.ip + 4 }

let op_code_8 m op =
  let param_1 = param_value m op 1
  and param_2 = param_value m op 2
  and addr = write_addr m op 3 in

  let value = if param_1 = param_2 then 1 else 0 in

  if m.debug then
    printf "%d EQL %d %d (%d) -> %d\n" m.ip param_1 param_2 value addr;

  let m = set_addr addr value m in
  { m with ip = m.ip + 4 }

let op_code_9 m op =
  let param_1 = param_value m op 1 in

  { m with ip = m.ip + 2; rel_base = m.rel_base + param_1 }

let op_code_99 m = { m with state = HALT; ip = m.ip + 1 }

let state_to_string s =
  match s with
  | HALT -> "HALT"
  | RUN -> "RUN"
  | INPUT -> "INPUT"
  | OUTPUT -> "OUTPUT"

let mach_to_string m =
  Printf.sprintf "{IP: %d STATE: %s}" m.ip @@ state_to_string m.state

let read_line input = try Some (input_line input) with End_of_file -> None

let parse_input file_name =
  let input = open_in file_name in
  match read_line input with
  | Some line ->
      String.split_on_char ',' line |> List.map int_of_string |> Array.of_list
  | None -> [||]

let halt_machine m = set_state m HALT

let step m =
  if m.state = HALT then raise @@ Failure "step: HALTED";
  let op = int_to_op m.prog.(m.ip) in

  if m.debug then printf "%d OP %d (%d)\n" m.ip m.prog.(m.ip) op.code;

  match op.code with
  | 1 -> op_code_1 m op
  | 2 -> op_code_2 m op
  | 3 -> op_code_3 m op
  | 4 -> op_code_4 m op
  | 5 -> op_code_5 m op
  | 6 -> op_code_6 m op
  | 7 -> op_code_7 m op
  | 8 -> op_code_8 m op
  | 9 -> op_code_9 m op
  | 99 -> op_code_99 m
  | _ -> raise @@ Invalid_argument (sprintf "UNHANDLED OP CODE %d\n" op.code)

let run_machine in_fn out_fn m =
  let rec loop mach =
    if get_state mach = HALT then mach
    else
      let mach = step mach in
      match get_state mach with
      | HALT -> mach
      | RUN -> loop mach
      | INPUT -> loop @@ in_fn mach
      | OUTPUT -> loop @@ out_fn mach
  in

  loop m
