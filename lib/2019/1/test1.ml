open OUnit2

let tests =
  "2019 Day Part 1" >::: [ ("checking" >:: fun _ -> assert_equal 1 1) ]

let _ = run_test_tt_main tests
