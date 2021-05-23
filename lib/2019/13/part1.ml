open Utils

let run file_name =
  let game = Intcode.parse_input file_name |> new_game |> run_game in

  Hashtbl.fold
    (fun _ tile acc -> if tile = BLOCK then acc + 1 else acc)
    game.grid 0
