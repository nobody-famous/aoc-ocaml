open OUnit2
open TestUtils

let part1_tests =
  "2022 Day 14 Part 1"
  >::: [
         ( "Sample" >:: fun _ ->
           [ "498,4 -> 498,6 -> 496,6"; "503,4 -> 502,4 -> 502,9 -> 494,9" ]
           |> Aoc_2022_14.Part1.run
           |> check_equal 24 );
       ]

let _ = run_test_tt_main part1_tests
