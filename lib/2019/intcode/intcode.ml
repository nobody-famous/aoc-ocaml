open Printf

type machine = { prog : int array; ndx : int; halt : bool; stdin : string list }

type param_mode = Position | Immediate

type op = { code : int; modes : param_mode array }

let new_machine prog = { prog; ndx = 0; halt = false; stdin = [] }

let param_mode_mask num =
  let rec loop value = function 1 -> value | n -> loop (value * 10) (n - 1) in

  loop 100 num

let get_param_mode instr num =
  let mask = param_mode_mask num in
  let digit = instr / mask mod 10 in
  match digit with 1 -> Immediate | _ -> Position

let int_to_op instr =
  {
    code = instr mod 100;
    modes =
      [|
        get_param_mode instr 1; get_param_mode instr 2; get_param_mode instr 3;
      |];
  }

let set_addr m addr value =
  m.prog.(addr) <- value;
  m

let get_addr m addr = m.prog.(addr)

let param_value m op offset =
  let v = m.prog.(m.ndx + offset) in
  if op.modes.(offset - 1) = Position then m.prog.(v) else v

let op_code_1 m op =
  let param_1 = param_value m op 1
  and param_2 = param_value m op 2
  and addr = m.prog.(m.ndx + 3) in

  m.prog.(addr) <- param_1 + param_2;
  { m with ndx = m.ndx + 4 }

let op_code_2 m op =
  let param_1 = param_value m op 1
  and param_2 = param_value m op 2
  and addr = m.prog.(m.ndx + 3) in

  m.prog.(addr) <- param_1 * param_2;
  { m with ndx = m.ndx + 4 }

let op_code_3 m _ =
  let _ = m.prog.(m.ndx + 1) in
  match m.stdin with
  | [] -> raise (Failure "NO INPUT")
  | _ -> { m with ndx = m.ndx + 2 }

let op_code_99 m = { m with halt = true; ndx = m.ndx + 1 }

let step m =
  let op = int_to_op m.prog.(m.ndx) in

  match op.code with
  | 1 -> op_code_1 m op
  | 2 -> op_code_2 m op
  | 3 -> op_code_3 m op
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
