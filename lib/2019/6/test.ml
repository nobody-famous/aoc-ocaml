open OUnit2

let check_equal x y = assert_equal x y ~printer:string_of_int

let part1_tests =
  "2019 Day 6 Part 1"
  >::: [
         ( "Sample" >:: fun _ ->
           [
             "COM)B";
             "B)C";
             "C)D";
             "D)E";
             "E)F";
             "B)G";
             "G)H";
             "D)I";
             "E)J";
             "J)K";
             "K)L";
           ]
           |> Aoc_2019_6.Part1.run
           |> check_equal 42 );
       ]

let part2_tests =
  "2019 Day 6 Part 2"
  >::: [
         ( "Sample" >:: fun _ ->
           [
             "COM)B";
             "B)C";
             "C)D";
             "D)E";
             "E)F";
             "B)G";
             "G)H";
             "D)I";
             "E)J";
             "J)K";
             "K)L";
             "K)YOU";
             "I)SAN";
           ]
           |> Aoc_2019_6.Part2.run
           |> check_equal 4 );
       ]

let _ = run_test_tt_main part1_tests
let _ = run_test_tt_main part2_tests
