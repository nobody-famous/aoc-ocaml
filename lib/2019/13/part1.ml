open Utils

let run file_name =
  let prog = Intcode.parse_input file_name in
  let game = new_game prog |> run_game in

  Hashtbl.fold
    (fun _ tile acc -> if tile = BLOCK then acc + 1 else acc)
    game.grid 0
