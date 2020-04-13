let segscan [n] 't (op: t -> t -> t) (ne: t)
                   (arr: [n](t, bool)): [n]t =
  let new_op = \(v1, f1) (v2, f2) -> (if f2 then v2 else op v1 v2, f1 || f2)
  let (res, _) = unzip (scan new_op (ne, false) arr)
  in res
  
