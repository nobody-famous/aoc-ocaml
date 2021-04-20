open Printf

type token = Int of int

let read_ch channel = try Some (input_char channel) with End_of_file -> None

let is_digit = function '0' .. '9' -> true | _ -> false

let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let char_value ch = Char.code ch - Char.code '0'

type parser = { input : in_channel; cur : char option }

let peek p = p.cur

let next p = { input = p.input; cur = read_ch p.input }

let parse_int p =
  let rec helper (p, t) =
    match p.cur with
    | Some c when is_digit c -> (
        match t with Int i -> helper (next p, Int ((i * 10) + char_value c)))
    | Some _ -> (p, Some t)
    | None -> (p, Some t)
  in
  helper (p, Int 0)

let get_token p =
  match p.cur with
  | Some c -> (
      match c with _ when is_digit c -> parse_int p | _ -> (p, None))
  | None -> (p, None)

let next_token = function
  | ch when is_digit ch -> printf "Digit %c\n" ch
  | ch when is_alpha ch -> printf "Alpha %c\n" ch
  | ch -> printf "Process %c\n" ch

let parse_input file_name =
  printf "Parse input %s\n" file_name;

  let p = { input = open_in file_name; cur = None } in

  let p, t = get_token (next p) in

  match t with
  | Some (Int t) ->
      printf "INT %d NEXT %c\n" t (match p.cur with Some c -> c | None -> 'X')
  | None -> printf "NONE\n"
