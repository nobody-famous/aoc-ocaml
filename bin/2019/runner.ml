open Printf

type problem = { y : int; d : int; part : int; fn : string -> int; exp : int }

let problems : problem list =
  [
    { y = 2019; d = 1; part = 1; fn = Aoc_2019_1.Part1.run; exp = 3279287 };
    { y = 2019; d = 1; part = 2; fn = Aoc_2019_1.Part2.run; exp = 4916076 };
    { y = 2019; d = 2; part = 1; fn = Aoc_2019_2.Part1.run; exp = 3101844 };
    { y = 2019; d = 2; part = 2; fn = Aoc_2019_2.Part2.run; exp = 8478 };
    { y = 2019; d = 3; part = 1; fn = Aoc_2019_3.Part1.run; exp = 529 };
    { y = 2019; d = 3; part = 2; fn = Aoc_2019_3.Part2.run; exp = 20386 };
    { y = 2019; d = 4; part = 1; fn = Aoc_2019_4.Part1.run; exp = 511 };
    { y = 2019; d = 4; part = 2; fn = Aoc_2019_4.Part2.run; exp = 316 };
    { y = 2019; d = 5; part = 1; fn = Aoc_2019_5.Part1.run; exp = 13294380 };
    { y = 2019; d = 5; part = 2; fn = Aoc_2019_5.Part2.run; exp = 11460760 };
    { y = 2019; d = 6; part = 1; fn = Aoc_2019_6.Part1.run; exp = 162816 };
    { y = 2019; d = 6; part = 2; fn = Aoc_2019_6.Part2.run; exp = 304 };
    { y = 2019; d = 7; part = 1; fn = Aoc_2019_7.Part1.run; exp = 21760 };
    { y = 2019; d = 7; part = 2; fn = Aoc_2019_7.Part2.run; exp = 69816958 };
    { y = 2019; d = 8; part = 1; fn = Aoc_2019_8.Part1.run; exp = 2356 };
    { y = 2019; d = 8; part = 2; fn = Aoc_2019_8.Part2.run; exp = 0 };
    { y = 2019; d = 9; part = 1; fn = Aoc_2019_9.Part1.run; exp = 2870072642 };
    { y = 2019; d = 9; part = 2; fn = Aoc_2019_9.Part2.run; exp = 58534 };
    { y = 2019; d = 10; part = 1; fn = Aoc_2019_10.Part1.run; exp = 347 };
    { y = 2019; d = 10; part = 2; fn = Aoc_2019_10.Part2.run; exp = 829 };
    { y = 2019; d = 11; part = 1; fn = Aoc_2019_11.Part1.run; exp = 1885 };
    { y = 2019; d = 11; part = 2; fn = Aoc_2019_11.Part2.run; exp = 0 };
    { y = 2019; d = 12; part = 1; fn = Aoc_2019_12.Part1.run; exp = 7077 };
    {
      y = 2019;
      d = 12;
      part = 2;
      fn = Aoc_2019_12.Part2.run;
      exp = 402951477454512;
    };
    { y = 2019; d = 13; part = 1; fn = Aoc_2019_13.Part1.run; exp = 251 };
    { y = 2019; d = 13; part = 2; fn = Aoc_2019_13.Part2.run; exp = 12779 };
    { y = 2019; d = 14; part = 1; fn = Aoc_2019_14.Part1.run; exp = 136771 };
    { y = 2019; d = 14; part = 2; fn = Aoc_2019_14.Part2.run; exp = 8193614 };
    { y = 2019; d = 15; part = 1; fn = Aoc_2019_15.Part1.run; exp = 254 };
    { y = 2019; d = 15; part = 2; fn = Aoc_2019_15.Part2.run; exp = 268 };
    { y = 2019; d = 16; part = 1; fn = Aoc_2019_16.Part1.run; exp = 68764632 };
    { y = 2019; d = 16; part = 2; fn = Aoc_2019_16.Part2.run; exp = 52825021 };
    { y = 2019; d = 17; part = 1; fn = Aoc_2019_17.Part1.run; exp = 8928 };
    { y = 2019; d = 17; part = 2; fn = Aoc_2019_17.Part2.run; exp = 880360 };
  ]

let timed_run p =
  let start = int_of_float (Unix.gettimeofday () *. 1000.0) in
  let file = sprintf "input/%d/%d/puzzle.txt" p.y p.d in
  let name = sprintf "%d.%d.%d" p.y p.d p.part in
  let actual = p.fn file in

  if actual <> p.exp then printf "%s FAILED: %d != %d\n" name actual p.exp;

  let diff = int_of_float (Unix.gettimeofday () *. 1000.0) - start in
  printf "%s took %d ms\n" name diff;

  diff

let time_all probs =
  let total = List.fold_left (fun acc fn -> acc + timed_run fn) 0 probs in

  Printf.printf "Total: %d ms\n" total
;;

time_all problems

(* Aoc_2019_18.Part1.run "input/2019/18/sample.txt" *)
