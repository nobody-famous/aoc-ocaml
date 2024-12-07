open OUnit2
open Aoc.Utils

let check_equal x y =
  match x with
  | IntResult xr -> (
      match y with
      | IntResult yr -> assert_equal xr yr ~printer:string_of_int
      | StringResult _ -> assert_failure "Expected an int, found a string")
  | StringResult xr -> (
      match y with
      | IntResult _ -> assert_failure "Expected a string, found an int"
      | StringResult yr -> assert_equal xr yr)
