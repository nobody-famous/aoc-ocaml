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
    {
      name = "2019.2.2";
      file = "input/2019/2/puzzle.txt";
      fn = Aoc_2019_2.Part2.run;
      exp = 8478;
    };
    {
      name = "2019.3.1";
      file = "input/2019/3/puzzle.txt";
      fn = Aoc_2019_3.Part1.run;
      exp = 529;
    };
    {
      name = "2019.3.2";
      file = "input/2019/3/puzzle.txt";
      fn = Aoc_2019_3.Part2.run;
      exp = 20386;
    };
    {
      name = "2019.4.1";
      file = "input/2019/4/puzzle.txt";
      fn = Aoc_2019_4.Part1.run;
      exp = 511;
    };
    {
      name = "2019.4.2";
      file = "input/2019/4/puzzle.txt";
      fn = Aoc_2019_4.Part2.run;
      exp = 316;
    };
    {
      name = "2019.5.1";
      file = "input/2019/5/puzzle.txt";
      fn = Aoc_2019_5.Part1.run;
      exp = 13294380;
    };
    {
      name = "2019.5.2";
      file = "input/2019/5/puzzle.txt";
      fn = Aoc_2019_5.Part2.run;
      exp = 11460760;
    };
    {
      name = "2019.6.1";
      file = "input/2019/6/puzzle.txt";
      fn = Aoc_2019_6.Part1.run;
      exp = 162816;
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

(* Aoc_2019_6.Part1.run "input/2019/6/sample.txt" *)
