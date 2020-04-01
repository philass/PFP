-- ==
-- input @ two_100_i32s
-- input @ two_1000_i32s
-- input @ two_10000_i32s
-- input @ two_100000_i32s
-- input @ two_1000000_i32s
-- input @ two_5000000_i32s
-- input @ two_10000000_i32s

let ilog2 x = 31 - i32.clz x

let update_function [n] (arr : [n]i32) (i: i32) (j: i32) : i32 =
  if j < 2 ** i
  then arr[j]
  else arr[j] + arr[j - 2 ** i]


let hillis_steele [n] (xs: [n]i32) : [n]i32 =
  unsafe
  let m = ilog2 n + 1
  in loop xs  = copy xs for d < m do
    map (update_function xs d) (iota n)

let main (xs: []i32) = hillis_steele xs
  
-- calculate index to be updated and values then use scatter
--
--let work_efficient [n] (xs:  [n]i32) : [n]i32 =
--  unsafe
--  let m = ilog2 n
--
--
--  let upswept = 0
--    loop xs = copy xs for  d in do
--
--  let upswept[n-1]= 0
--
--
--  let downswept =
--    loop xs = upswept for d in ... do
--
--
--
--  in downswept
