type robot = { position : int * int; velocity : int * int }

let re_patterns lines = (Re.Perl.compile_pat "p=(\\d+),(\\d+)\\s+v=(-?\\d+),(-?\\d+)", lines)

let to_robot group =
  {
    position = (int_of_string @@ Re.Group.get group 1, int_of_string @@ Re.Group.get group 2);
    velocity = (int_of_string @@ Re.Group.get group 3, int_of_string @@ Re.Group.get group 4);
  }

let parse_robot robot_re line = line |> Re.exec robot_re |> to_robot
let parse_robots (robot_re, lines) = List.map (fun line -> parse_robot robot_re line) lines
let parse_input lines = lines |> re_patterns |> parse_robots
