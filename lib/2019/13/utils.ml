type next_output = X_POS | Y_POS | VALUE

type instr = { x : int; y : int; value : int }

type game_piece = EMPTY | WALL | BLOCK | PADDLE | BALL

type coords = { x : int; y : int }

type 'a game_state = {
  mach : 'a Intcode.machine;
  next_out : next_output;
  cur_instr : instr;
  score : int;
  paddle : coords;
  ball : coords;
  grid : (coords, game_piece) Hashtbl.t;
}

let new_game prog =
  {
    mach = Intcode.new_machine () prog;
    next_out = X_POS;
    cur_instr = { x = 0; y = 0; value = 0 };
    score = 0;
    paddle = { x = 0; y = 0 };
    ball = { x = 0; y = 0 };
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
  let piece = piece_of_id game.cur_instr.value in
  let game =
    if piece = PADDLE then { game with paddle = point }
    else if piece = BALL then { game with ball = point }
    else game
  in

  Hashtbl.replace game.grid point piece;
  game

let update_score game = { game with score = game.cur_instr.value }

let proc_out game out =
  let next = inc_next_output game.next_out in
  let game =
    match game.next_out with
    | X_POS -> { game with cur_instr = { game.cur_instr with x = out } }
    | Y_POS -> { game with cur_instr = { game.cur_instr with y = out } }
    | VALUE ->
        let game =
          { game with cur_instr = { game.cur_instr with value = out } }
        in

        if game.cur_instr.x = -1 && game.cur_instr.y = 0 then update_score game
        else update_tile game
  in

  { game with next_out = next }

let machine_output game =
  let m, out = Intcode.get_output game.mach in
  let game' = { game with mach = m } in

  let game' =
    match out with
    | None -> raise @@ Failure "Expected output, but had none"
    | Some v -> proc_out game' v
  in

  game'

let machine_input game =
  let ball = game.ball in
  let paddle = game.paddle in

  let game =
    if ball.x < paddle.x then
      { game with mach = Intcode.set_input game.mach (-1) }
    else if ball.x > paddle.x then
      { game with mach = Intcode.set_input game.mach 1 }
    else { game with mach = Intcode.set_input game.mach 0 }
  in

  game

let run_game game =
  let rec loop g =
    let m = g.mach |> Intcode.step in
    let g = { g with mach = m } in

    match Intcode.get_state m with
    | HALT -> g
    | RUN -> loop { g with mach = m }
    | OUTPUT -> loop @@ machine_output g
    | INPUT -> loop @@ machine_input g
  in

  loop game
