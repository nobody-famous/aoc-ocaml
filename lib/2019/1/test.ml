open OUnit2
open TestUtils

let part1_tests =
  "2019 Day 1 Part 1"
  >::: [
         ( "sample" >:: fun _ ->
           [ "12"; "14"; "1969"; "100756" ] |> Aoc_2019_1.Part1.run |> check_equal @@ Aoc.Utils.IntResult 34241 );
       ]

let part2_tests =
  "2019 Day 1 Part 2"
  >::: [
         ( "sample" >:: fun _ ->
           [ "12"; "14"; "1969"; "100756" ] |> Aoc_2019_1.Part2.run |> check_equal @@ Aoc.Utils.IntResult 51316 );
       ]

let _ = run_test_tt_main part1_tests
let _ = run_test_tt_main part2_tests
