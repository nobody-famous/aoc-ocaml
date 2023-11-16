open OUnit2
open TestUtils
module G = Aoc.Graph

let graph_tests =
  "Graph Tests"
  >::: [
         ( "Example" >:: fun _ ->
           [
             ( 1,
               {
                 G.edges =
                   [
                     { G.target = 2; G.weight = 7 };
                     { G.target = 3; G.weight = 9 };
                     { G.target = 6; G.weight = 14 };
                   ];
               } );
             ( 2,
               {
                 G.edges =
                   [
                     { G.target = 1; G.weight = 7 };
                     { G.target = 3; G.weight = 10 };
                     { G.target = 4; G.weight = 15 };
                   ];
               } );
             ( 3,
               {
                 G.edges =
                   [
                     { G.target = 1; G.weight = 9 };
                     { G.target = 2; G.weight = 10 };
                     { G.target = 4; G.weight = 11 };
                     { G.target = 6; G.weight = 2 };
                   ];
               } );
             ( 4,
               {
                 G.edges =
                   [
                     { G.target = 2; G.weight = 15 };
                     { G.target = 3; G.weight = 11 };
                     { G.target = 5; G.weight = 6 };
                   ];
               } );
             ( 5,
               {
                 G.edges =
                   [
                     { G.target = 4; G.weight = 6 };
                     { G.target = 6; G.weight = 9 };
                   ];
               } );
             ( 6,
               {
                 G.edges =
                   [
                     { G.target = 1; G.weight = 14 };
                     { G.target = 3; G.weight = 2 };
                     { G.target = 5; G.weight = 9 };
                   ];
               } );
           ]
           |> List.to_seq
           |> Hashtbl.of_seq
           |> G.shortest_path ~start_pos:1 ~end_pos:5 ~init_weight:0
           |> fun node -> node.weight |> check_equal 20 );
       ]

let _ = run_test_tt_main graph_tests
