type position = { row : int; col : int }
type cellData = { pos : position; ch : char }
type grid = { startPos : position; endPos : position; cells : cellData list }

let toCellData row (line : string) =
  line
  |> String.to_seq
  |> List.of_seq
  |> List.mapi (fun col ch -> { pos = { row; col }; ch })

let toGrid data =
  List.fold_left
    (fun grid item ->
      match item.ch with
      | 'S' ->
          {
            grid with
            startPos = item.pos;
            cells = { pos = item.pos; ch = 'a' } :: grid.cells;
          }
      | 'E' ->
          {
            grid with
            endPos = item.pos;
            cells = { pos = item.pos; ch = 'z' } :: grid.cells;
          }
      | _ -> { grid with cells = item :: grid.cells })
    {
      startPos = { row = 0; col = 0 };
      endPos = { row = 0; col = 0 };
      cells = [];
    }
    data

let parse_input lines = List.mapi toCellData lines |> List.flatten |> toGrid
