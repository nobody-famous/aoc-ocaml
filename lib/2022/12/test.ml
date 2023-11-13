open OUnit2
open TestUtils

let part1_tests =
  "2022 Day 12 Part 1"
  >::: [
         ( "Sample" >:: fun _ ->
           [ "Sabqponm"; "abcryxxl"; "accszExk"; "acctuvwj"; "abdefghi" ]
           |> Aoc_2022_12.Part1.run
           |> check_equal 31 );
       ]

let _ = run_test_tt_main part1_tests
