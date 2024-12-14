let re_patterns lines = (Re.Perl.compile_pat "(\\d+):(.*)", Re.Perl.compile_pat "(\\d+)", lines)

let split_equation eq_re line =
  let groups = Re.exec eq_re line in
  (int_of_string @@ Re.Group.get groups 1, Re.Group.get groups 2)

let parse_nums numbers_re (value, nums_str) =
  let matches = Re.all numbers_re nums_str in
  (value, List.map (fun groups -> int_of_string @@ Re.Group.get groups 1) matches)

let parse_equation eq_re numbers_re line = line |> split_equation eq_re |> parse_nums numbers_re
let parse_equations (eq_re, numbers_re, lines) = List.map (fun line -> parse_equation eq_re numbers_re line) lines
let parse_input lines = lines |> re_patterns |> parse_equations
