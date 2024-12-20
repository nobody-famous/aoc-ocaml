open Parser
open Utils

let count_orbits start tree =
  let counts = Hashtbl.create (Hashtbl.length tree) in
  let rec loop node =
    let count = if Hashtbl.mem counts node then Hashtbl.find counts node else 0 in

    let kids = try Hashtbl.find_all tree node with Not_found -> [] in
    match kids with
    | [] -> ()
    | _ ->
        List.iter
          (fun k ->
            Hashtbl.replace counts k (count + 1);
            loop k)
          kids
  in

  loop start;
  counts

let run lines =
  let counts = parse_input lines |> create_tree |> count_orbits "COM" in

  Aoc.Utils.IntResult (Hashtbl.fold (fun _ count acc -> acc + count) counts 0)
