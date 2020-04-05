open import "lib/github.com/diku-dk/sorts/radix_sort"

let sort_wrapper [n] 'a (to_sort : [n]a)  key_fun : [n]i32 = 
	radix_sort_by_key key_fun i32.num_bits i32.get_bit (iota (n))

--let helper1 [n] 'a (to_sort : [n]a)  : [n]i32 = 
--	radix_sort i32.num_bits i32.get_bit to_sort

--let helper2 'a [m] [n] [t] (dest : [m]a)
--	(is : [n]i32) (as : [n]a) : ([t]a, [t]i32) = ((concat dest as), (concat (iota m) is))

--Make flag array (index new rows)
let mk_flags  [n] (vals: [n]i32): [n]bool =
  let idxs = iota n
  let vals = map (\i ->   if i == 0
                          then true
                          else (vals[i-1] != vals[i])) idxs
  in vals


let bastard_zip [k][n] 't (m : i32) (v: [k]t) (a : [n]bool) : [m](t, bool)  = map (\i -> (v[i], a[i])) (iota m)

let seg_reduce [n] 't (op: t -> t -> t) (ne: t)
                   (arr: [n](t, bool)): []t =
  let (_, flags) = unzip arr
  let new_op = \(v1, f1) (v2, f2) -> (if f2 then v2 else op v1 v2, f1 || f2)
  let (res, _) = unzip (scan new_op (ne, false) arr)
  let idx_to_keep = rotate 1 flags
  let (reductions, _) = unzip (filter  (\(_, i) -> i) (zip res idx_to_keep))
  in reductions

let iota_helper [m] 't (temp : [m]t) : [m]i32 = iota m

let reduce_by_indexx 'a [m] [n] (dest : *[m]a)
	(f : a -> a -> a) (ne : a)
	(is : [n]i32) (as : [n]a) : *[m]a = 
	 let fp = zip (iota m) dest
	 let sp = zip is as
	 let whole = concat fp sp
	 let (idx_to_sort, vals) = unzip whole
	 let key_fun i = idx_to_sort[i]
	 let idx_of_rel = sort_wrapper idx_to_sort key_fun
	 let to_flag = map (\i -> vals[idx_of_rel[i]]) (iota_helper vals)
	 let to_flag_i = map (\i -> idx_to_sort[idx_of_rel[i]]) (iota_helper idx_to_sort)
	 let flags = mk_flags to_flag_i
	 let temp = seg_reduce f ne (zip to_flag flags)
	 in scatter dest (iota_helper temp) temp
	
