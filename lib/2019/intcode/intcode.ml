open Printf

(* type machine_state = HALT | NEED_INPUT | HAS_OUTPUT *)
type machine_state = HALT | RUN

type machine = {
  prog : int array;
  ip : int;
  stdin : unit -> int;
  stdout : int -> unit;
  state : machine_state;
  debug : bool;
}

type param_mode = Position | Immediate

type op = { code : int; modes : param_mode array }

let new_machine prog =
  {
    prog;
    ip = 0;
    stdin = (fun () -> 0);
    stdout = (fun _ -> ());
    state = RUN;
    debug = false;
  }

let new_machine_io prog in_fn out_fn =
  { prog; ip = 0; stdin = in_fn; stdout = out_fn; state = RUN; debug = false }

let get_param_mode instr mask =
  let digit = instr / mask mod 10 in
  match digit with 1 -> Immediate | _ -> Position

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

let set_addr m addr value =
  m.prog.(addr) <- value;
  m

let set_stdin m fn = { m with stdin = fn }

let set_stdout m fn = { m with stdout = fn }

let set_debug m value = { m with debug = value }

let get_addr m addr = m.prog.(addr)

let param_value m op offset =
  let v = m.prog.(m.ip + offset) in
  let value = if op.modes.(offset - 1) = Position then m.prog.(v) else v in

  value

let math_op label m op fn =
  let param_1 = param_value m op 1
  and param_2 = param_value m op 2
  and addr = m.prog.(m.ip + 3) in
  let value = fn param_1 param_2 in

  if m.debug then
    printf "%s %d %d = (%d) -> %d\n" label param_1 param_2 value addr;

  m.prog.(addr) <- value;
  { m with ip = m.ip + 4 }

let op_code_1 m op = math_op "ADD" m op (fun a b -> a + b)

let op_code_2 m op = math_op "MUL" m op (fun a b -> a * b)

let op_code_3 m _ =
  let addr = m.prog.(m.ip + 1) in
  let input = m.stdin () in

  m.prog.(addr) <- input;

  if m.debug then printf "INP %d -> %d\n" m.prog.(addr) addr;

  { m with ip = m.ip + 2 }

let op_code_4 m op =
  let addr = m.prog.(m.ip + 1) in
  let value = param_value m op 1 in
  let fn = m.stdout in

  if m.debug then printf "OUT %d (%d)\n" addr value;

  fn value;
  { m with ip = m.ip + 2 }

let jmp m op fn =
  let param_1 = param_value m op 1 and param_2 = param_value m op 2 in

  if m.debug then printf "JMP %d %d\n" param_1 param_2;

  if fn param_1 then { m with ip = param_2 } else { m with ip = m.ip + 3 }

let op_code_5 m op = jmp m op (fun n -> n <> 0)

let op_code_6 m op = jmp m op (fun n -> n = 0)

let op_code_7 m op =
  let param_1 = param_value m op 1
  and param_2 = param_value m op 2
  and addr = m.prog.(m.ip + 3) in

  let value = if param_1 < param_2 then 1 else 0 in

  if m.debug then printf "LT %d %d (%d) -> %d\n" param_1 param_2 value addr;

  m.prog.(addr) <- value;
  { m with ip = m.ip + 4 }

let op_code_8 m op =
  let param_1 = param_value m op 1
  and param_2 = param_value m op 2
  and addr = m.prog.(m.ip + 3) in

  let value = if param_1 = param_2 then 1 else 0 in

  if m.debug then printf "EQL %d %d (%d) -> %d\n" param_1 param_2 value addr;

  m.prog.(addr) <- value;
  { m with ip = m.ip + 4 }

let op_code_99 m = { m with state = HALT; ip = m.ip + 1 }

let step m =
  if m.state = HALT then raise (Failure "Machine Halted");
  let op = int_to_op m.prog.(m.ip) in

  if m.debug then printf "OP %d (%d)\n" m.prog.(m.ip) op.code;

  match op.code with
  | 1 -> op_code_1 m op
  | 2 -> op_code_2 m op
  | 3 -> op_code_3 m op
  | 4 -> op_code_4 m op
  | 5 -> op_code_5 m op
  | 6 -> op_code_6 m op
  | 7 -> op_code_7 m op
  | 8 -> op_code_8 m op
  | 99 -> op_code_99 m
  | _ -> raise (Invalid_argument (sprintf "UNHANDLED OP CODE %d\n" op.code))

let rec run_prog m =
  match m.state = HALT with true -> m | false -> run_prog (step m)

let mach_to_string m = Printf.sprintf "{IP: %d HALT: %b}" m.ip (m.state = HALT)

let read_line input = try Some (input_line input) with End_of_file -> None

let parse_input file_name =
  let input = open_in file_name in
  match read_line input with
  | Some line ->
      String.split_on_char ',' line |> List.map int_of_string |> Array.of_list
  | None -> [||]
