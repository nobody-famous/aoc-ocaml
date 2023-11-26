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
         ( "Parse list" >:: fun _ ->
           "[]" |> Aoc_2022_13.Parser.parse_line |> fun (rem, value) ->
           assert_equal ~msg:"Remaining" rem [];
           assert_equal ~msg:"Value" value (Aoc_2022_13.Utils.PacketList []) );
         ( "Parse nested list" >:: fun _ ->
           "[[]]" |> Aoc_2022_13.Parser.parse_line |> fun (rem, value) ->
           assert_equal ~msg:"Remaining" rem [];
           assert_equal ~msg:"Value" value
             (Aoc_2022_13.Utils.PacketList [ Aoc_2022_13.Utils.PacketList [] ])
         );
         ( "Parse multiple nested lists" >:: fun _ ->
           "[[],[]]" |> Aoc_2022_13.Parser.parse_line |> fun (rem, value) ->
           assert_equal ~msg:"Remaining" rem [];
           assert_equal ~msg:"Value" value
             (Aoc_2022_13.Utils.PacketList
                [
                  Aoc_2022_13.Utils.PacketList [];
                  Aoc_2022_13.Utils.PacketList [];
                ]) );
         ( "Parse number list" >:: fun _ ->
           "[45]" |> Aoc_2022_13.Parser.parse_line |> fun (rem, value) ->
           assert_equal ~msg:"Remaining" rem [];
           assert_equal ~msg:"Value" value
             (Aoc_2022_13.Utils.PacketList [ Aoc_2022_13.Utils.Integer 45 ]) );
         ( "Parse multiple numbers list" >:: fun _ ->
           "[45,2]" |> Aoc_2022_13.Parser.parse_line |> fun (rem, value) ->
           assert_equal ~msg:"Remaining" rem [];
           assert_equal ~msg:"Value" value
             (Aoc_2022_13.Utils.PacketList
                [ Aoc_2022_13.Utils.Integer 45; Aoc_2022_13.Utils.Integer 2 ])
         );
         ( "Nested list with content" >:: fun _ ->
           "[1,[45,2]]" |> Aoc_2022_13.Parser.parse_line |> fun (rem, value) ->
           assert_equal ~msg:"Remaining" rem [];
           assert_equal ~msg:"Value" value
             (Aoc_2022_13.Utils.PacketList
                [
                  Aoc_2022_13.Utils.Integer 1;
                  Aoc_2022_13.Utils.PacketList
                    [
                      Aoc_2022_13.Utils.Integer 45; Aoc_2022_13.Utils.Integer 2;
                    ];
                ]) );
         ( "Parse example line" >:: fun _ ->
           "[1,[2,[3,[4,[5,6,7]]]],8,9]" |> Aoc_2022_13.Parser.parse_line
           |> fun (rem, value) ->
           assert_equal ~msg:"Remaining" rem [];
           assert_equal ~msg:"Value" value
             (Aoc_2022_13.Utils.PacketList
                [
                  Aoc_2022_13.Utils.Integer 1;
                  Aoc_2022_13.Utils.PacketList
                    [
                      Aoc_2022_13.Utils.Integer 2;
                      Aoc_2022_13.Utils.PacketList
                        [
                          Aoc_2022_13.Utils.Integer 3;
                          Aoc_2022_13.Utils.PacketList
                            [
                              Aoc_2022_13.Utils.Integer 4;
                              Aoc_2022_13.Utils.PacketList
                                [
                                  Aoc_2022_13.Utils.Integer 5;
                                  Aoc_2022_13.Utils.Integer 6;
                                  Aoc_2022_13.Utils.Integer 7;
                                ];
                            ];
                        ];
                    ];
                  Aoc_2022_13.Utils.Integer 8;
                  Aoc_2022_13.Utils.Integer 9;
                ]) );
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
           |> check_equal 13 );
       ]

let _ = run_test_tt_main part1_tests
