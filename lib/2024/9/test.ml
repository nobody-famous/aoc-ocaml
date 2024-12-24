open OUnit2
open TestUtils

let lines = [ "2333133121414131402" ]

let part1_tests =
  "2024 Day 9 Part 1"
  >::: [ ("Sample" >:: fun _ -> lines |> Aoc_2024_9.Part1.run |> check_equal @@ Aoc.Utils.IntResult 1928) ]

(* let part2_tests =
   "2024 Day 9 Part 2"
   >::: [ ("Sample" >:: fun _ -> lines |> Aoc_2024_9.Part2.run |> check_equal @@ Aoc.Utils.IntResult 34) ] *)

let _ = run_test_tt_main part1_tests
(* let _ = run_test_tt_main part2_tests *)
