let init_ptrs disk =
  let left_ptr = Array.find_index (fun item -> item = -1) disk in
  let right_ptr = Array.length disk - 1 in
  (left_ptr, right_ptr, disk)

let run lines =
  let _ = lines |> Parser.parse_input |> init_ptrs in
  Aoc.Utils.IntResult 0
