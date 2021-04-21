open Printf

type problem = { name : string; file : string; fn : string -> int; exp : int }

let fns : problem list =
  [
    {
      name = "2019.1.1";
      file = "input/2019/1/puzzle.txt";
      fn = Aoc_2019_1.Part1.run;
      exp = 3279287;
    };
    {
      name = "2019.1.2";
      file = "input/2019/1/puzzle.txt";
      fn = Aoc_2019_1.Part2.run;
      exp = 4916076;
    };
    {
      name = "2019.2.1";
      file = "input/2019/2/puzzle.txt";
      fn = Aoc_2019_2.Part1.run;
      exp = 3101844;
    };
  ]

let timed_run (p : problem) =
  let start = int_of_float (Unix.gettimeofday () *. 1000.0) in

  let actual = p.fn p.file in
  if actual <> p.exp then printf "%s FAILED: %d != %d\n" p.name actual p.exp;

  let diff = int_of_float (Unix.gettimeofday () *. 1000.0) - start in
  printf "%s took %d ms\n" p.name diff

;;
List.iter timed_run fns

(* Aoc_2019_2.Part1.run "input/2019/2/puzzle.txt" *)
