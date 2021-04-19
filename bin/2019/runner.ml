open Printf

type problem = {
  name : string;
  file : string;
  fn : string -> int64;
  exp : int64;
}

;;
let fns : problem list =
  [
    {
      name = "2019.1.1";
      file = "input/2019/1/puzzle.txt";
      fn = Aoc_2019_1.Part1.run;
      exp = 3279287L;
    };
    {
      name = "2019.1.2";
      file = "input/2019/1/sample.txt";
      fn = Aoc_2019_1.Part2.run;
      exp = 0L;
    };
  ]
in

let foo (p : problem) =
  let start = int_of_float (Unix.gettimeofday () *. 1000.0) in

  let actual = p.fn p.file in
  if actual <> p.exp then printf "%s FAILED: %Ld != %Ld\n" p.name actual p.exp;

  let diff = int_of_float (Unix.gettimeofday () *. 1000.0) - start in
  printf "%s took %d ms\n" p.name diff
in

List.iter foo fns
