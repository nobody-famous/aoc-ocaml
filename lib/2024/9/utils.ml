type fileInfo = { id : int; size : int }
type block = File of fileInfo | Empty of int

let init_ptrs blocks = (0, Array.length blocks - 1, blocks)

let checksum input =
  let sum_range first last =
    let first_float = float_of_int first in
    let last_float = float_of_int last in
    int_of_float @@ ((first_float +. last_float) /. 2. *. (last_float -. first_float +. 1.))
  in

  let rec do_checksum pos blocks total =
    match blocks with
    | block :: rest -> (
        match block with
        | Empty s -> do_checksum (pos + s) rest total
        | File f -> do_checksum (pos + f.size) rest (total + (f.id * sum_range pos (pos + f.size - 1))))
    | _ -> total
  in

  do_checksum 0 input 0
