open Utils

let run lines =
  let input = Parser.parse_input lines in
  let ore_target = 1000000000000 in

  let rec loop low high =
    let mid = low + ((high - low) / 2) in
    let ore = calc_ore mid input in

    if low = mid then low
    else if ore > ore_target then loop low mid
    else if ore < ore_target then loop mid high
    else mid
  in

  loop 0 ore_target
