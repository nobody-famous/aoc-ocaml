open Utils

let update_tile game =
  let point = { x = game.cur_instr.x; y = game.cur_instr.y } in
  let piece = piece_of_id game.cur_instr.tile_id in

  Hashtbl.replace game.grid point piece

let proc_out game out =
  let next = inc_next_output game.next_out in
  let game =
    match game.next_out with
    | X_POS -> { game with cur_instr = { game.cur_instr with x = out } }
    | Y_POS -> { game with cur_instr = { game.cur_instr with y = out } }
    | VALUE ->
        let game =
          { game with cur_instr = { game.cur_instr with tile_id = out } }
        in

        update_tile game;
        game
  in

  { game with next_out = next }

let run file_name =
  let prog = Intcode.parse_input file_name in
  let game = new_game prog |> run_game proc_out in

  Hashtbl.fold
    (fun _ tile acc -> if tile = BLOCK then acc + 1 else acc)
    game.grid 0
