open OUnit2
open TestUtils

let lines =
  [
    "190: 10 19";
    "3267: 81 40 27";
    "83: 17 5";
    "156: 15 6";
    "7290: 6 8 6 15";
    "161011: 16 10 13";
    "192: 17 8 14";
    "21037: 9 7 18 13";
    "292: 11 6 16 20";
  ]

let part1_tests =
  "2024 Day 7 Part 1"
  >::: [ ("Sample" >:: fun _ -> lines |> Aoc_2024_7.Part1.run |> check_equal @@ Aoc.Utils.IntResult 3749) ]

let part2_tests =
   "2024 Day 7 Part 2"
   >::: [ ("Sample" >:: fun _ -> lines |> Aoc_2024_7.Part2.run |> check_equal @@ Aoc.Utils.IntResult 11387) ]

(* let _ = run_test_tt_main part1_tests *)
let _ = run_test_tt_main part2_tests
