open Utils

let run lines =
  let game = Intcode.parse_input lines |> new_game |> run_game in

  Hashtbl.fold
    (fun _ tile acc -> if tile = BLOCK then acc + 1 else acc)
    game.grid 0
