module Point = struct
  type t = int * int

  let compare (a_row, a_col) (b_row, b_col) =
    if a_row < b_row || (a_row = b_row && a_col < b_col) then -1 else if a_row = b_row && a_col = b_col then 0 else 1
end

module Points = Set.Make (Point)
module Grid = Map.Make (Point)
