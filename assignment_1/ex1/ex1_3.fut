-- ==
-- input @ two_100_i32s
-- input @ two_1000_i32s
-- input @ two_10000_i32s
-- input @ two_100000_i32s
-- input @ two_1000000_i32s
-- input @ two_5000000_i32s
-- input @ two_10000000_i32s

let process_idx [n] (s1: [n]i32) (s2: [n]i32) : (i32, i32) = 
  let f = \x y -> i32.abs (x - y)
  let abs_difs = map2 f s1 s2

  let g = (\(v1, idx1) (v2, idx2) -> if v1 > v2 then (v1, idx1)
                                else if v2 > v1 then (v2, idx2)
                                else if idx1 > idx2 then (v1, idx1)
                                else                 (v2, idx2))
  in reduce_comm g (0, -1) (zip abs_difs (iota n))

let main (xs: []i32) (ys: []i32) = process_idx xs ys

