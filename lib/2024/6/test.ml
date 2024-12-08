open OUnit2
open TestUtils

let test_data =
  [
    "....#.....";
    ".........#";
    "..........";
    "..#.......";
    ".......#..";
    "..........";
    ".#..^.....";
    "........#.";
    "#.........";
    "......#...";
  ]

let part1_tests =
  "2024 Day 6 Part 1"
  >::: [ ("Sample" >:: fun _ -> test_data |> Aoc_2024_6.Part1.run |> check_equal @@ Aoc.Utils.IntResult 41) ]

let part2_tests =
  "2024 Day 6 Part 2"
  >::: [ ("Sample" >:: fun _ -> test_data |> Aoc_2024_6.Part2.run |> check_equal @@ Aoc.Utils.IntResult 6) ]

let _ = run_test_tt_main part1_tests
let _ = run_test_tt_main part2_tests
