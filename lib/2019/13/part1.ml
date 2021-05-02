type next_output = X_POS | Y_POS | VALUE

type instr = { x : int; y : int; tile_id : int }

type game_piece = EMPTY | WALL | BLOCK | PADDLE | BALL

type coords = { x : int; y : int }

type game_state = {
  mach : Intcode.machine;
  next_out : next_output;
  cur_instr : instr;
  grid : (coords, game_piece) Hashtbl.t;
}

let new_game prog =
  {
    mach = Intcode.new_machine prog;
    next_out = X_POS;
    cur_instr = { x = 0; y = 0; tile_id = 0 };
    grid = Hashtbl.create 64;
  }

let inc_next_output out =
  match out with X_POS -> Y_POS | Y_POS -> VALUE | VALUE -> X_POS

let piece_of_id id =
  match id with
  | 0 -> EMPTY
  | 1 -> WALL
  | 2 -> BLOCK
  | 3 -> PADDLE
  | 4 -> BALL
  | _ -> raise @@ Failure (Printf.sprintf "Invalid ID %d" id)

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

let run_game game =
  let rec loop g =
    let m = g.mach in
    let m = Intcode.step m in

    match Intcode.get_state m with
    | HALT -> g
    | RUN -> loop { g with mach = m }
    | OUTPUT ->
        let m, out = Intcode.get_output m in
        let game' = { g with mach = m } in

        let game' =
          match out with
          | None -> raise @@ Failure "Expected output, but had none"
          | Some v -> proc_out game' v
        in

        loop game'
    | s ->
        raise
        @@ Failure (Printf.sprintf "UNHANDLED %s" @@ Intcode.state_to_string s)
  in

  loop game

let run file_name =
  let prog = Intcode.parse_input file_name in
  let game = new_game prog |> run_game in

  Hashtbl.fold
    (fun _ tile acc -> if tile = BLOCK then acc + 1 else acc)
    game.grid 0
