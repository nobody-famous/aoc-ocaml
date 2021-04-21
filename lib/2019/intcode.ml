open Printf

type machine = { prog : int array; ndx : int; halt : bool }

let new_machine prog = { prog; ndx = 0; halt = false }

let set_addr m addr value =
  m.prog.(addr) <- value;
  m

let get_addr m addr = m.prog.(addr)

let step m =
  let op_code, addr_1, addr_2, addr_3 =
    (m.prog.(m.ndx), m.prog.(m.ndx + 1), m.prog.(m.ndx + 2), m.prog.(m.ndx + 3))
  in

  match op_code with
  | 1 ->
      m.prog.(addr_3) <- m.prog.(addr_1) + m.prog.(addr_2);
      { m with ndx = m.ndx + 4 }
  | 2 ->
      m.prog.(addr_3) <- m.prog.(addr_1) * m.prog.(addr_2);
      { m with ndx = m.ndx + 4 }
  | 99 -> { m with halt = true; ndx = m.ndx + 1 }
  | _ ->
      printf "op_code %d %d %d %d\n" op_code addr_1 addr_2 addr_3;
      m

let rec run_prog m = match m.halt with true -> m | false -> run_prog (step m)
