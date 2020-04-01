-- ==
-- input @ two_100_i32s
-- input @ two_1000_i32s
-- input @ two_10000_i32s
-- input @ two_100000_i32s
-- input @ two_1000000_i32s
-- input @ two_5000000_i32s
-- input @ two_10000000_i32s

let process [n] (s1: [n]i32) (s2: [n]i32) : i32 = 
  let f = \x y -> i32.abs (x - y)
  let abs_difs = map2 f s1 s2
  in reduce i32.max 0 abs_difs

let main (xs: []i32) (ys: []i32) = process xs ys
