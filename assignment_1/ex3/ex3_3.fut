

let seg_reduce [n] 't (op: t -> t -> t) (ne: t)
                   (arr: [n](t, bool)): []t =
  let (_, flags) = unzip arr
  let new_op = \(v1, f1) (v2, f2) -> (if f2 then v2 else op v1 v2, f1 || f2)
  let (res, _) = unzip (scan new_op (ne, false) arr)
  let idx_to_keep = rotate 1 flags
  let (reductions, _) = unzip (filter  (\(_, i) -> i) (zip res idx_to_keep))
  in reductions
