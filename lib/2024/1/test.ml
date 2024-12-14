open OUnit2
open TestUtils

let lines = [ "3   4"; "4   3"; "2   5"; "1   3"; "3   9"; "3   3" ]

let part1_tests =
  "2024 Day 1 Part 1"
  >::: [ ("Sample" >:: fun _ -> lines |> Aoc_2024_1.Part1.run |> check_equal @@ Aoc.Utils.IntResult 11) ]

let part2_tests =
  "2024 Day 1 Part 2"
  >::: [ ("Sample" >:: fun _ -> lines |> Aoc_2024_1.Part2.run |> check_equal @@ Aoc.Utils.IntResult 31) ]

let _ = run_test_tt_main part1_tests
let _ = run_test_tt_main part2_tests
