type chemical = { amount : int; name : string }

type input_chems = { amount : int; chems : chemical list }

let parse_chem chem_str =
  let str = Str.regexp "\\([0-9]+\\) \\([A-Z]+\\)" in

  if Str.string_match str chem_str 0 then
    let amount = int_of_string @@ Str.matched_group 1 chem_str in
    let name = Str.matched_group 2 chem_str in

    { amount; name }
  else raise @@ Failure (Printf.sprintf "Invalid chemical %s" chem_str)

let parse_line ht line =
  let str = Str.regexp "\\(.*\\) => \\([0-9]+\\) \\(.*\\)" in

  if Str.string_match str line 0 then
    let in_chems_str = Str.matched_group 1 line in
    let out_chem =
      {
        amount = int_of_string @@ Str.matched_group 2 line;
        name = Str.matched_group 3 line;
      }
    in
    let splits = Str.split (Str.regexp ", ") in_chems_str in
    let in_chems = List.map parse_chem splits in

    Hashtbl.replace ht out_chem.name
      { amount = out_chem.amount; chems = in_chems }
  else raise @@ Failure (Printf.sprintf "Invalid input %s" line)

let parse_input file_name =
  let lines = InputParser.read_lines file_name in
  let ht = Hashtbl.create 64 in

  List.iter (fun line -> parse_line ht line) lines;

  ht
