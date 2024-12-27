(* let next_empty_block start disk =
     let rec do_find index disk =
       if index >= Array.length disk then
         None
       else if disk.(index) = -1 then
         Some index
       else
         do_find (index + 1) disk
     in
     do_find start disk

   let init_ptrs disk =
     let left_ptr = next_empty_block 0 disk in
     let right_ptr = Array.length disk - 1 in
     (left_ptr, right_ptr, disk)

   let defrag (left, right, disk) =
     let rec do_defrag left_ptr right_ptr disk =
       match left_ptr with
       | Some ptr when ptr < right_ptr ->
           disk.(ptr) <- disk.(right_ptr);
           disk.(right_ptr) <- -1;
           do_defrag (next_empty_block ptr disk) (right_ptr - 1) disk
       | Some ptr when ptr >= right_ptr -> disk
       | _ -> disk
     in
     do_defrag left right disk *)

let checksum disk =
  disk |> Array.fold_left (fun (i, sum) v -> (i + 1, if v >= 0 then sum + (i * v) else sum)) (0, 0) |> fun (_, s) -> s

let init_lists blocks =
  let get_files blocks =
    List.fold_left
      (fun acc block ->
        match block with
        | Parser.File f -> f :: acc
        | Parser.Empty _ -> acc)
      [] blocks
  in

  let get_empty blocks =
    List.fold_left
      (fun acc block ->
        match block with
        | Parser.File _ -> acc
        | Parser.Empty e -> e :: acc)
      [] blocks
  in

  (get_empty blocks |> List.rev, get_files blocks |> List.rev, get_files blocks)

let defrag (empty, files_forward, files_backward) =
  let rec do_defrag use_forward blocks rem_empty rem_forward rem_backward =
    if use_forward then
      match rem_forward with
      | hd :: rest -> do_defrag false (Parser.File hd :: blocks) rem_empty rest rem_backward
      | [] -> blocks
    else
      match rem_backward with
      | f :: rest_backward -> (
          match rem_empty with
          | e :: rest_empty ->
              if e < f.Parser.size then
                let new_block = { f with size = e } in
                let new_backward = { f with size = f.size - e } in
                do_defrag true (Parser.File new_block :: blocks) rest_empty rem_forward (new_backward :: rest_backward)
              else if e > f.size then
                let new_empty = e - f.size in
                do_defrag false (Parser.File f :: blocks) (new_empty :: rest_empty) rem_forward rest_backward
              else
                do_defrag true (Parser.File f :: blocks) rest_empty rem_forward rest_backward
          | [] -> blocks)
      | [] -> blocks
  in

  do_defrag true [] empty files_forward files_backward |> List.rev

(* let run lines = Aoc.Utils.IntResult (lines |> Parser.parse_input |> init_ptrs |> defrag |> checksum) *)
let run lines =
  let blocks = lines |> Parser.parse_input |> init_lists |> defrag in
  Printf.printf "***** BLOCKS\n";
  List.iter
    (fun block ->
      match block with
      | Parser.File f -> Printf.printf "*****  FILE %d %d\n" f.id f.size
      | Parser.Empty e -> Printf.printf "*****  EMPTY %d" e)
    blocks;
  Aoc.Utils.IntResult 0
