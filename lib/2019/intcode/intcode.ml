open Printf

type machine = {
  prog : int array;
  ndx : int;
  halt : bool;
  stdin : string list;
  stdout : int -> unit;
  debug : bool;
}

type param_mode = Position | Immediate

type op = { code : int; modes : param_mode array }

let new_machine prog =
  {
    prog;
    ndx = 0;
    halt = false;
    stdin = [];
    stdout = (fun _ -> ());
    debug = false;
  }

let new_machine_io prog input out_fn =
  { prog; ndx = 0; halt = false; stdin = input; stdout = out_fn; debug = false }

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

let set_addr m addr value =
  m.prog.(addr) <- value;
  m

let set_debug m value = { m with debug = value }

let get_addr m addr = m.prog.(addr)

let param_value m op offset =
  let v = m.prog.(m.ndx + offset) in
  let value = if op.modes.(offset - 1) = Position then m.prog.(v) else v in

  value

let math_op label m op fn =
  let param_1 = param_value m op 1
  and param_2 = param_value m op 2
  and addr = m.prog.(m.ndx + 3) in
  let value = fn param_1 param_2 in

  if m.debug then
    printf "%s %d %d = (%d) -> %d\n" label param_1 param_2 value addr;

  m.prog.(addr) <- value;
  { m with ndx = m.ndx + 4 }

let op_code_1 m op = math_op "ADD" m op (fun a b -> a + b)

let op_code_2 m op = math_op "MUL" m op (fun a b -> a * b)

let op_code_3 m _ =
  let addr = m.prog.(m.ndx + 1) in
  match m.stdin with
  | [] -> raise (Failure "NO INPUT")
  | next :: rest ->
      m.prog.(addr) <- int_of_string next;

      if m.debug then printf "INP %d -> %d\n" m.prog.(addr) addr;

      { m with ndx = m.ndx + 2; stdin = rest }

let op_code_4 m op =
  let addr = m.prog.(m.ndx + 1) in
  let value = param_value m op 1 in
  let fn = m.stdout in

  if m.debug then printf "OUT %d (%d)\n" addr value;

  fn value;
  { m with ndx = m.ndx + 2 }

let jmp m op fn =
  let param_1 = param_value m op 1 and param_2 = param_value m op 2 in

  if m.debug then printf "JMP %d %d\n" param_1 param_2;

  if fn param_1 then { m with ndx = param_2 } else { m with ndx = m.ndx + 3 }

let op_code_5 m op = jmp m op (fun n -> n <> 0)

let op_code_6 m op = jmp m op (fun n -> n = 0)

let op_code_7 m op =
  let param_1 = param_value m op 1
  and param_2 = param_value m op 2
  and addr = m.prog.(m.ndx + 3) in

  let value = if param_1 < param_2 then 1 else 0 in

  if m.debug then printf "LT %d %d (%d) -> %d\n" param_1 param_2 value addr;

  m.prog.(addr) <- value;
  { m with ndx = m.ndx + 4 }

let op_code_8 m op =
  let param_1 = param_value m op 1
  and param_2 = param_value m op 2
  and addr = m.prog.(m.ndx + 3) in

  let value = if param_1 = param_2 then 1 else 0 in

  if m.debug then printf "EQL %d %d (%d) -> %d\n" param_1 param_2 value addr;

  m.prog.(addr) <- value;
  { m with ndx = m.ndx + 4 }

let op_code_99 m = { m with halt = true; ndx = m.ndx + 1 }

let step m =
  let op = int_to_op m.prog.(m.ndx) in

  if m.debug then printf "OP %d (%d)\n" m.prog.(m.ndx) op.code;

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

let rec run_prog m = match m.halt with true -> m | false -> run_prog (step m)

let read_line input = try Some (input_line input) with End_of_file -> None

let parse_input file_name =
  let input = open_in file_name in
  match read_line input with
  | Some line ->
      String.split_on_char ',' line |> List.map int_of_string |> Array.of_list
  | None -> [||]
