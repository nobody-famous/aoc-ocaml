open Printf

type problem = { name : string; file : string; fn : string -> int; exp : int }

let problems : problem list =
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
    {
      name = "2019.6.2";
      file = "input/2019/6/puzzle.txt";
      fn = Aoc_2019_6.Part2.run;
      exp = 304;
    };
    {
      name = "2019.7.1";
      file = "input/2019/7/puzzle.txt";
      fn = Aoc_2019_7.Part1.run;
      exp = 21760;
    };
    {
      name = "2019.7.2";
      file = "input/2019/7/puzzle.txt";
      fn = Aoc_2019_7.Part2.run;
      exp = 69816958;
    };
    {
      name = "2019.8.1";
      file = "input/2019/8/puzzle.txt";
      fn = Aoc_2019_8.Part1.run;
      exp = 2356;
    };
    {
      name = "2019.8.2";
      file = "input/2019/8/puzzle.txt";
      fn = Aoc_2019_8.Part2.run;
      exp = 0;
    };
    {
      name = "2019.9.1";
      file = "input/2019/9/puzzle.txt";
      fn = Aoc_2019_9.Part1.run;
      exp = 2870072642;
    };
    {
      name = "2019.9.2";
      file = "input/2019/9/puzzle.txt";
      fn = Aoc_2019_9.Part2.run;
      exp = 58534;
    };
    {
      name = "2019.10.1";
      file = "input/2019/10/puzzle.txt";
      fn = Aoc_2019_10.Part1.run;
      exp = 347;
    };
    {
      name = "2019.10.2";
      file = "input/2019/10/puzzle.txt";
      fn = Aoc_2019_10.Part2.run;
      exp = 829;
    };
    {
      name = "2019.11.1";
      file = "input/2019/11/puzzle.txt";
      fn = Aoc_2019_11.Part1.run;
      exp = 1885;
    };
    {
      name = "2019.11.2";
      file = "input/2019/11/puzzle.txt";
      fn = Aoc_2019_11.Part2.run;
      exp = 0;
    };
    {
      name = "2019.12.1";
      file = "input/2019/12/puzzle.txt";
      fn = Aoc_2019_12.Part1.run;
      exp = 7077;
    };
    {
      name = "2019.12.2";
      file = "input/2019/12/puzzle.txt";
      fn = Aoc_2019_12.Part2.run;
      exp = 402951477454512;
    };
    {
      name = "2019.13.1";
      file = "input/2019/13/puzzle.txt";
      fn = Aoc_2019_13.Part1.run;
      exp = 251;
    };
    {
      name = "2019.13.2";
      file = "input/2019/13/puzzle.txt";
      fn = Aoc_2019_13.Part2.run;
      exp = 12779;
    };
    {
      name = "2019.14.1";
      file = "input/2019/14/puzzle.txt";
      fn = Aoc_2019_14.Part1.run;
      exp = 136771;
    };
    {
      name = "2019.14.2";
      file = "input/2019/14/puzzle.txt";
      fn = Aoc_2019_14.Part2.run;
      exp = 8193614;
    };
    {
      name = "2019.15.1";
      file = "input/2019/15/puzzle.txt";
      fn = Aoc_2019_15.Part1.run;
      exp = 254;
    };
  ]

let timed_run p =
  let start = int_of_float (Unix.gettimeofday () *. 1000.0) in
  let actual = p.fn p.file in

  if actual <> p.exp then printf "%s FAILED: %d != %d\n" p.name actual p.exp;

  let diff = int_of_float (Unix.gettimeofday () *. 1000.0) - start in
  printf "%s took %d ms\n" p.name diff;

  diff

let time_all probs =
  let total = List.fold_left (fun acc fn -> acc + timed_run fn) 0 probs in

  Printf.printf "Total: %d ms\n" total

;;
time_all problems

(* Aoc_2019_15.Part1.run "input/2019/15/puzzle.txt" *)
