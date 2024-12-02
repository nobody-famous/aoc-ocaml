open OUnit2
open TestUtils

let part1_tests =
  "2019 Day 3 Part 1"
  >::: [
         ( "Sample 1" >:: fun _ ->
           [ "R8,U5,L5,D3"; "U7,R6,D4,L4" ] |> Aoc_2019_3.Part1.run |> check_equal @@ Aoc.Utils.IntResult 6 );
         ( "Sample 2" >:: fun _ ->
           [ "R75,D30,R83,U83,L12,D49,R71,U7,L72"; "U62,R66,U55,R34,D71,R55,D58,R83" ]
           |> Aoc_2019_3.Part1.run
           |> check_equal @@ Aoc.Utils.IntResult 159 );
         ( "Sample 3" >:: fun _ ->
           [ "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"; "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" ]
           |> Aoc_2019_3.Part1.run
           |> check_equal @@ Aoc.Utils.IntResult 135 );
       ]

let part2_tests =
  "2019 Day 3 Part 2"
  >::: [
         ( "Sample 1" >:: fun _ ->
           [ "R8,U5,L5,D3"; "U7,R6,D4,L4" ] |> Aoc_2019_3.Part2.run |> check_equal @@ Aoc.Utils.IntResult 30 );
         ( "Sample 2" >:: fun _ ->
           [ "R75,D30,R83,U83,L12,D49,R71,U7,L72"; "U62,R66,U55,R34,D71,R55,D58,R83" ]
           |> Aoc_2019_3.Part2.run
           |> check_equal @@ Aoc.Utils.IntResult 610 );
         ( "Sample 3" >:: fun _ ->
           [ "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"; "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" ]
           |> Aoc_2019_3.Part2.run
           |> check_equal @@ Aoc.Utils.IntResult 410 );
       ]

let _ = run_test_tt_main part1_tests
let _ = run_test_tt_main part2_tests
