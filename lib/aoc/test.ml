open OUnit2
open TestUtils
module G = Aoc.Graph

let pair_to_edge (t, w) = { G.target = t; G.weight = w }
let make_node pos edges = (pos, { G.edges = List.map pair_to_edge edges })

let graph_tests =
  "Graph Tests"
  >::: [
         ( "Example" >:: fun _ ->
           [
             make_node 1 [ (2, 7); (3, 9); (6, 14) ];
             make_node 2 [ (1, 7); (3, 10); (4, 15) ];
             make_node 3 [ (1, 9); (2, 10); (4, 11); (6, 2) ];
             make_node 4 [ (2, 15); (3, 11); (5, 6) ];
             make_node 5 [ (4, 6); (6, 9) ];
             make_node 6 [ (1, 14); (3, 2); (5, 9) ];
           ]
           |> List.to_seq
           |> Hashtbl.of_seq
           |> G.shortest_path ~start_pos:1 ~end_pos:5 ~init_weight:0
           |> fun node -> node.weight |> check_equal 20 );
       ]

let _ = run_test_tt_main graph_tests
