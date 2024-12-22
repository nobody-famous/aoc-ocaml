let rec all_pts fn pts grid diff pt =
  if Utils.on_grid (fst pt) (snd pt) grid then
    all_pts fn (pt :: pts) grid diff @@ fn pt diff
  else
    pts

let pair_antinodes grid (left, right) =
  let diff = Utils.pt_diff left right in
  List.concat [ all_pts Utils.add_pt [] grid diff left; all_pts Utils.sub_pt [] grid diff left ]

let run lines = Utils.run pair_antinodes lines
