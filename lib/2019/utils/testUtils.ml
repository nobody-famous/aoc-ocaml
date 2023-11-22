open OUnit2

let check_equal x y = assert_equal x y ~printer:string_of_int
