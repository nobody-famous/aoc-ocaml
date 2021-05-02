type direction = UP | DOWN | RIGHT | LEFT

type color = BLACK | WHITE

type out_type = COLOR | DIRECTION

type location = { row : int; col : int }

type robot = {
  mach : Intcode.machine;
  dir : direction;
  loc : location;
  seen : (location, color) Hashtbl.t;
  out : out_type;
}

let new_robot prog =
  {
    mach = Intcode.new_machine prog;
    dir = UP;
    loc = { row = 0; col = 0 };
    seen = Hashtbl.create 64;
    out = COLOR;
  }

let dir_to_string dir =
  match dir with
  | UP -> "UP"
  | DOWN -> "DOWN"
  | RIGHT -> "RIGHT"
  | LEFT -> "LEFT"

let color_to_string c = match c with BLACK -> "BLACK" | WHITE -> "WHITE"

let out_to_string o = match o with COLOR -> "COLOR" | DIRECTION -> "DIR"

let robot_to_string bot =
  Printf.sprintf "{DIR: %s LOC: %d,%d OUT: %s}" (dir_to_string bot.dir)
    bot.loc.row bot.loc.col (out_to_string bot.out)

let get_color_input bot =
  let color = try Hashtbl.find bot.seen bot.loc with Not_found -> BLACK in
  match color with BLACK -> 0 | WHITE -> 1

let rotate dir out =
  match dir with
  | UP -> if out = 0 then LEFT else RIGHT
  | DOWN -> if out = 0 then RIGHT else LEFT
  | LEFT -> if out = 0 then DOWN else UP
  | RIGHT -> if out = 0 then UP else DOWN

let move bot =
  match bot.dir with
  | UP -> { bot with loc = { row = bot.loc.row - 1; col = bot.loc.col } }
  | DOWN -> { bot with loc = { row = bot.loc.row + 1; col = bot.loc.col } }
  | RIGHT -> { bot with loc = { row = bot.loc.row; col = bot.loc.col + 1 } }
  | LEFT -> { bot with loc = { row = bot.loc.row; col = bot.loc.col - 1 } }

let mark_seen bot color = Hashtbl.replace bot.seen bot.loc color

let proc_output bot out =
  match bot.out with
  | COLOR ->
      let color = if out = 1 then WHITE else BLACK in
      mark_seen bot color;
      { bot with out = DIRECTION }
  | DIRECTION ->
      let new_dir = rotate bot.dir out in
      let bot = { bot with dir = new_dir; out = COLOR } in
      move bot

let run_robot in_bot in_color =
  let rec loop bot =
    let bot = { bot with mach = Intcode.step bot.mach } in

    match Intcode.get_state bot.mach with
    | HALT -> ()
    | RUN -> loop bot
    | INPUT ->
        let m = Intcode.set_input bot.mach (get_color_input bot) in
        loop { bot with mach = m }
    | OUTPUT -> (
        let m, out = Intcode.get_output bot.mach in
        match out with
        | None -> raise @@ Failure "NO OUTPUT"
        | Some v -> loop (proc_output { bot with mach = m } v))
  in

  mark_seen in_bot in_color;
  loop in_bot
