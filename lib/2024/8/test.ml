open OUnit2
open TestUtils

let lines =
  [
    "............";
    "........0...";
    ".....0......";
    ".......0....";
    "....0.......";
    "......A.....";
    "............";
    "............";
    "........A...";
    ".........A..";
    "............";
    "............";
  ]

let part1_tests =
  "2024 Day 8 Part 1"
  >::: [ ("Sample" >:: fun _ -> lines |> Aoc_2024_8.Part1.run |> check_equal @@ Aoc.Utils.IntResult 14) ]

(* let part2_tests =
   "2024 Day 8 Part 2"
   >::: [ ("Sample" >:: fun _ -> lines |> Aoc_2024_8.Part2.run |> check_equal @@ Aoc.Utils.IntResult 11387) ] *)

let _ = run_test_tt_main part1_tests
(* let _ = run_test_tt_main part2_tests *)
