type block = { pos : int; size : int }
type file = { id : int; block : block }

let init_ptrs disk = (0, Array.length disk - 1, disk)

(* let checksum in_disk =
   let rec do_checksum pos total disk =
     if pos >= Array.length disk then
       total
     else
       let new_pos = pos + 1 in
       let new_total = if disk.(pos) = -1 then total else total + (disk.(pos) * pos) in
       do_checksum new_pos new_total disk
   in

   do_checksum 0 0 in_disk *)

let sum_range x y =
  let float_x = float_of_int x in
  let float_y = float_of_int y in
  int_of_float ((float_x +. float_y) /. 2. *. (float_y -. float_x +. 1.))

let checksum files =
  let file_cost file = sum_range file.block.pos (file.block.pos + (file.block.size - 1)) in
  List.fold_left (fun acc file -> acc + (file.id * file_cost file)) 0 files
