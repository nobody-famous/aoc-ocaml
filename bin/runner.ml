open Printf
open Aoc.Utils

type problem = { label : string; file : string; fn : string list -> result; exp : result }
type year = { year : int; probs : problem list }

let years : year list =
  [
    {
      year = 2019;
      probs =
        [
          { label = "1.1"; file = "2019/1"; fn = Aoc_2019_1.Part1.run; exp = IntResult 3279287 };
          { label = "1.2"; file = "2019/1"; fn = Aoc_2019_1.Part2.run; exp = IntResult 4916076 };
          { label = "2.1"; file = "2019/2"; fn = Aoc_2019_2.Part1.run; exp = IntResult 3101844 };
          { label = "2.2"; file = "2019/2"; fn = Aoc_2019_2.Part2.run; exp = IntResult 8478 };
          { label = "3.1"; file = "2019/3"; fn = Aoc_2019_3.Part1.run; exp = IntResult 529 };
          { label = "3.2"; file = "2019/3"; fn = Aoc_2019_3.Part2.run; exp = IntResult 20386 };
          { label = "4.1"; file = "2019/4"; fn = Aoc_2019_4.Part1.run; exp = IntResult 511 };
          { label = "4.2"; file = "2019/4"; fn = Aoc_2019_4.Part2.run; exp = IntResult 316 };
          { label = "5.1"; file = "2019/5"; fn = Aoc_2019_5.Part1.run; exp = IntResult 13294380 };
          { label = "5.2"; file = "2019/5"; fn = Aoc_2019_5.Part2.run; exp = IntResult 11460760 };
          { label = "6.1"; file = "2019/6"; fn = Aoc_2019_6.Part1.run; exp = IntResult 162816 };
          { label = "6.2"; file = "2019/6"; fn = Aoc_2019_6.Part2.run; exp = IntResult 304 };
          { label = "7.1"; file = "2019/7"; fn = Aoc_2019_7.Part1.run; exp = IntResult 21760 };
          { label = "7.2"; file = "2019/7"; fn = Aoc_2019_7.Part2.run; exp = IntResult 69816958 };
          { label = "8.1"; file = "2019/8"; fn = Aoc_2019_8.Part1.run; exp = IntResult 2356 };
          { label = "8.2"; file = "2019/8"; fn = Aoc_2019_8.Part2.run; exp = IntResult 0 };
          { label = "9.1"; file = "2019/9"; fn = Aoc_2019_9.Part1.run; exp = IntResult 2870072642 };
          { label = "9.2"; file = "2019/9"; fn = Aoc_2019_9.Part2.run; exp = IntResult 58534 };
          { label = "10.1"; file = "2019/10"; fn = Aoc_2019_10.Part1.run; exp = IntResult 347 };
          { label = "10.2"; file = "2019/10"; fn = Aoc_2019_10.Part2.run; exp = IntResult 829 };
          { label = "11.1"; file = "2019/11"; fn = Aoc_2019_11.Part1.run; exp = IntResult 1885 };
          { label = "11.2"; file = "2019/11"; fn = Aoc_2019_11.Part2.run; exp = IntResult 0 };
          { label = "12.1"; file = "2019/12"; fn = Aoc_2019_12.Part1.run; exp = IntResult 7077 };
          { label = "12.2"; file = "2019/12"; fn = Aoc_2019_12.Part2.run; exp = IntResult 402951477454512 };
          { label = "13.1"; file = "2019/13"; fn = Aoc_2019_13.Part1.run; exp = IntResult 251 };
          { label = "13.2"; file = "2019/13"; fn = Aoc_2019_13.Part2.run; exp = IntResult 12779 };
          { label = "14.1"; file = "2019/14"; fn = Aoc_2019_14.Part1.run; exp = IntResult 136771 };
          { label = "14.2"; file = "2019/14"; fn = Aoc_2019_14.Part2.run; exp = IntResult 8193614 };
          { label = "15.1"; file = "2019/15"; fn = Aoc_2019_15.Part1.run; exp = IntResult 254 };
          { label = "15.2"; file = "2019/15"; fn = Aoc_2019_15.Part2.run; exp = IntResult 268 };
          { label = "16.1"; file = "2019/16"; fn = Aoc_2019_16.Part1.run; exp = IntResult 68764632 };
          { label = "16.2"; file = "2019/16"; fn = Aoc_2019_16.Part2.run; exp = IntResult 52825021 };
          { label = "17.1"; file = "2019/17"; fn = Aoc_2019_17.Part1.run; exp = IntResult 8928 };
          { label = "17.2"; file = "2019/17"; fn = Aoc_2019_17.Part2.run; exp = IntResult 880360 };
        ];
    };
    {
      year = 2022;
      probs =
        [
          { label = "12.1"; file = "2022/12"; fn = Aoc_2022_12.Part1.run; exp = IntResult 370 };
          { label = "12.2"; file = "2022/12"; fn = Aoc_2022_12.Part2.run; exp = IntResult 363 };
          { label = "13.1"; file = "2022/13"; fn = Aoc_2022_13.Part1.run; exp = IntResult 5185 };
          { label = "13.2"; file = "2022/13"; fn = Aoc_2022_13.Part2.run; exp = IntResult 23751 };
        ];
    };
  ]

let get_result actual expected =
  let ok =
    match expected with
    | IntResult e -> (
        match actual with
        | IntResult a -> e = a
        | StringResult _ -> false)
    | StringResult e -> (
        match actual with
        | IntResult _ -> false
        | StringResult a -> e = a)
  in
  if ok then "OK" else "FAIL"

let timed_run fn file =
  let start = int_of_float (Unix.gettimeofday () *. 1000.0) and actual = fn file in
  let diff = int_of_float (Unix.gettimeofday () *. 1000.0) - start in

  (diff, actual)

let time_prob prob =
  let diff, actual = sprintf "input/%s/puzzle.txt" prob.file |> InputParser.read_lines |> timed_run prob.fn in
  let result = get_result actual prob.exp in

  printf "[%s] %s %d ms\n" result prob.label diff;
  diff

let time_year (y : year) =
  printf "Year %d\n" y.year;

  let total = List.fold_left (fun acc p -> acc + time_prob p) 0 y.probs in

  printf "Total: %d ms\n\n" total

let _ = years |> List.iter time_year
