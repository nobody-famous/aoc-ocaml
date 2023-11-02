type stats = { zeros : int; ones : int; twos : int }

let calc_stats m =
  let rec row_loop row stats =
    let rec col_loop col stats =
      if col >= Array.length m.(row) then stats
      else
        match m.(row).(col) with
        | '0' -> col_loop (col + 1) { stats with zeros = stats.zeros + 1 }
        | '1' -> col_loop (col + 1) { stats with ones = stats.ones + 1 }
        | '2' -> col_loop (col + 1) { stats with twos = stats.twos + 1 }
        | _ -> col_loop (col + 1) stats
    in

    if row < Array.length m then
      let stats' = col_loop 0 stats in
      row_loop (row + 1) stats'
    else stats
  in

  row_loop 0 { zeros = 0; ones = 0; twos = 0 }

let calc_answer stats = stats.ones * stats.twos

let run lines =
  Parser.parse_input lines 25 6
  |> List.fold_left
       (fun cur m ->
         let stats = calc_stats m in
         if stats.zeros < cur.zeros then stats else cur)
       { zeros = max_int; ones = 0; twos = 0 }
  |> calc_answer
