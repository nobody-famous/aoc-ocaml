open Printf

type problem = { d : int; part : int; fn : string list -> int; exp : int }
type year = { label : int; probs : problem list }

let years : year list =
  [
    {
      label = 2019;
      probs =
        [
          { d = 1; part = 1; fn = Aoc_2019_1.Part1.run; exp = 3279287 };
          { d = 1; part = 2; fn = Aoc_2019_1.Part2.run; exp = 4916076 };
          { d = 2; part = 1; fn = Aoc_2019_2.Part1.run; exp = 3101844 };
          { d = 2; part = 2; fn = Aoc_2019_2.Part2.run; exp = 8478 };
          { d = 3; part = 1; fn = Aoc_2019_3.Part1.run; exp = 529 };
          { d = 3; part = 2; fn = Aoc_2019_3.Part2.run; exp = 20386 };
          { d = 4; part = 1; fn = Aoc_2019_4.Part1.run; exp = 511 };
          { d = 4; part = 2; fn = Aoc_2019_4.Part2.run; exp = 316 };
          { d = 5; part = 1; fn = Aoc_2019_5.Part1.run; exp = 13294380 };
          { d = 5; part = 2; fn = Aoc_2019_5.Part2.run; exp = 11460760 };
          { d = 6; part = 1; fn = Aoc_2019_6.Part1.run; exp = 162816 };
          { d = 6; part = 2; fn = Aoc_2019_6.Part2.run; exp = 304 };
          { d = 7; part = 1; fn = Aoc_2019_7.Part1.run; exp = 21760 };
          { d = 7; part = 2; fn = Aoc_2019_7.Part2.run; exp = 69816958 };
          { d = 8; part = 1; fn = Aoc_2019_8.Part1.run; exp = 2356 };
          { d = 8; part = 2; fn = Aoc_2019_8.Part2.run; exp = 0 };
          { d = 9; part = 1; fn = Aoc_2019_9.Part1.run; exp = 2870072642 };
          { d = 9; part = 2; fn = Aoc_2019_9.Part2.run; exp = 58534 };
          { d = 10; part = 1; fn = Aoc_2019_10.Part1.run; exp = 347 };
          { d = 10; part = 2; fn = Aoc_2019_10.Part2.run; exp = 829 };
          { d = 11; part = 1; fn = Aoc_2019_11.Part1.run; exp = 1885 };
          { d = 11; part = 2; fn = Aoc_2019_11.Part2.run; exp = 0 };
          { d = 12; part = 1; fn = Aoc_2019_12.Part1.run; exp = 7077 };
          {
            d = 12;
            part = 2;
            fn = Aoc_2019_12.Part2.run;
            exp = 402951477454512;
          };
          { d = 13; part = 1; fn = Aoc_2019_13.Part1.run; exp = 251 };
          { d = 13; part = 2; fn = Aoc_2019_13.Part2.run; exp = 12779 };
          { d = 14; part = 1; fn = Aoc_2019_14.Part1.run; exp = 136771 };
          { d = 14; part = 2; fn = Aoc_2019_14.Part2.run; exp = 8193614 };
          { d = 15; part = 1; fn = Aoc_2019_15.Part1.run; exp = 254 };
          { d = 15; part = 2; fn = Aoc_2019_15.Part2.run; exp = 268 };
          { d = 16; part = 1; fn = Aoc_2019_16.Part1.run; exp = 68764632 };
          { d = 16; part = 2; fn = Aoc_2019_16.Part2.run; exp = 52825021 };
          { d = 17; part = 1; fn = Aoc_2019_17.Part1.run; exp = 8928 };
          { d = 17; part = 2; fn = Aoc_2019_17.Part2.run; exp = 880360 };
        ];
    };
    {
      label = 2022;
      probs =
        [
          { d = 12; part = 1; fn = Aoc_2022_12.Part1.run; exp = 370 };
          { d = 12; part = 2; fn = Aoc_2022_12.Part2.run; exp = 363 };
          { d = 13; part = 1; fn = Aoc_2022_13.Part1.run; exp = 5185 };
          { d = 13; part = 2; fn = Aoc_2022_13.Part2.run; exp = 23751 };
        ];
    };
  ]

let get_label actual expected =
  if actual = expected then "OK" else sprintf "FAIL %d <> %d" actual expected

let timed_run fn file =
  let start = int_of_float (Unix.gettimeofday () *. 1000.0)
  and actual = fn file in

  let diff = int_of_float (Unix.gettimeofday () *. 1000.0) - start in

  (diff, actual)

let time_prob year prob =
  let file = sprintf "input/%d/%d/puzzle.txt" year prob.d
  and name = sprintf "%d.%d" prob.d prob.part in

  let diff, actual = InputParser.read_lines file |> timed_run prob.fn in
  let label = get_label actual prob.exp in

  printf "[%s] %s %d ms\n" label name diff;
  diff

let time_year (y : year) =
  printf "Year %d\n" y.label;

  let total =
    List.fold_left (fun acc p -> acc + time_prob y.label p) 0 y.probs
  in

  printf "Total: %d ms\n\n" total

let all_years years = List.iter time_year years;;

all_years years
