open OUnit2
open TestUtils

let lines =
  [
    "p=0,4 v=3,-3";
    "p=6,3 v=-1,-3";
    "p=10,3 v=-1,2";
    "p=2,0 v=2,-1";
    "p=0,0 v=1,3";
    "p=3,0 v=-2,-2";
    "p=7,6 v=-1,-3";
    "p=3,0 v=-1,-2";
    "p=9,3 v=2,3";
    "p=7,3 v=-1,2";
    "p=2,4 v=2,-3";
    "p=9,5 v=-3,-3";
  ]

let part1_tests =
  "2024 Day 14 Part 1"
  >::: [ ("Sample" >:: fun _ -> lines |> Aoc_2024_14.Part1.run_sized 11 7 |> check_equal @@ Aoc.Utils.IntResult 12) ]

let _ = run_test_tt_main part1_tests
