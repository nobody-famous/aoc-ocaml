let pair_antinodes _ (left, right) =
  let diff = Utils.pt_diff left right in
  [ Utils.add_pt left diff; Utils.sub_pt right diff ]

let run lines = Utils.run pair_antinodes lines
