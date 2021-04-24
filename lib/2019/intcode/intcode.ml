open Printf

type machine = { prog : int array; ndx : int; halt : bool }

let new_machine prog = { prog; ndx = 0; halt = false }

let set_addr m addr value =
  m.prog.(addr) <- value;
  m

let get_addr m addr = m.prog.(addr)

let op_code_1 m =
  let addr_1, addr_2, addr_3 =
    (m.prog.(m.ndx + 1), m.prog.(m.ndx + 2), m.prog.(m.ndx + 3))
  in

  m.prog.(addr_3) <- m.prog.(addr_1) + m.prog.(addr_2);
  { m with ndx = m.ndx + 4 }

let op_code_2 m =
  let addr_1, addr_2, addr_3 =
    (m.prog.(m.ndx + 1), m.prog.(m.ndx + 2), m.prog.(m.ndx + 3))
  in

  m.prog.(addr_3) <- m.prog.(addr_1) * m.prog.(addr_2);
  { m with ndx = m.ndx + 4 }

let op_code_99 m = { m with halt = true; ndx = m.ndx + 1 }

let step m =
  let op_code = m.prog.(m.ndx) in

  match op_code with
  | 1 -> op_code_1 m
  | 2 -> op_code_2 m
  | 99 -> op_code_99 m
  | _ ->
      printf "unahndled op_code %d\n" op_code;
      m

let rec run_prog m = match m.halt with true -> m | false -> run_prog (step m)
