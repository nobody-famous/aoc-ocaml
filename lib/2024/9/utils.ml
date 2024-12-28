type fileInfo = { id : int; size : int }
type block = File of fileInfo | Empty of int

let init_ptrs disk = (0, Array.length disk - 1, disk)

let checksum in_disk =
  let rec do_checksum pos total disk =
    if pos >= Array.length disk then
      total
    else
      let new_pos = pos + 1 in
      let new_total = if disk.(pos) = -1 then total else total + (disk.(pos) * pos) in
      do_checksum new_pos new_total disk
  in

  do_checksum 0 0 in_disk
