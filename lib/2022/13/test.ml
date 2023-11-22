open OUnit2
open TestUtils

let part1_tests =
  "Part1 Tests"
  >::: [
         ( "Parse number" >:: fun _ ->
           [ '1'; '2'; '3' ] |> Aoc_2022_13.Parser.parse_num
           |> fun (rem, value) ->
           assert_equal rem [];
           assert_equal value 123 );
         ( "Parse number remaining" >:: fun _ ->
           [ '1'; '2'; '3'; ',' ] |> Aoc_2022_13.Parser.parse_num
           |> fun (rem, value) ->
           assert_equal rem [ ',' ];
           assert_equal value 123 );
         ( "Example" >:: fun _ ->
           [
             "[1,1,3,1,1]";
             "[1,1,5,1,1]";
             "";
             "[[1],[2,3,4]]";
             "[[1],4]";
             "";
             "[9]";
             "[[8,7,6]]";
             "";
             "[[4,4],4,4]";
             "[[4,4],4,4,4]";
             "";
             "[7,7,7,7]";
             "[7,7,7]";
             "";
             "[]";
             "[3]";
             "";
             "[[[]]]";
             "[[]]";
             "";
             "[1,[2,[3,[4,[5,6,7]]]],8,9]";
             "[1,[2,[3,[4,[5,6,0]]]],8,9]";
           ]
           |> Aoc_2022_13.Part1.run
           |> check_equal 1 );
       ]

let _ = run_test_tt_main part1_tests
