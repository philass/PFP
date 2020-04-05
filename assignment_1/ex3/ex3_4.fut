open import "lib/github.com/diku-dk/sorts/radix_sort"

let helper [n] 'a (to_sort : [n]a)  key_fun : [n]i32 = 
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
	 let to_sort = concat dest as
	 let idx_to_sort = concat (iota m) is
	-- let (idx_sort, idx_to_sort) = helper2 dest is as 
	let key_fun idx = idx_to_sort[idx]
	-- let sorted_is = helper to_sort key_fun
	--let sorted_iss = helper idx_to_sort key_fun
	--let cop = copy sorted_iss
	let c1 = copy to_sort
	let c2 = copy idx_to_sort
	let mapc1 = map (\i -> to_sort[i]) c1
	let mapc2 = map (\i -> idx_to_sort[i]) c2
	let tester = helper c1 key_fun
	let as_vals = scatter (copy to_sort) (iota (length mapc1)) mapc1
	--let as_vals = scatter (copy to_sort) (helper c1 key_fun) c1
	let idx_vals = scatter (copy idx_to_sort) (helper c2 key_fun) c2
	let flags = mk_flags idx_vals
	let flag_v = bastard_zip m as_vals flags
	let temp = (seg_reduce f ne flag_v)
	let v = break 3 + 3
	in scatter dest (iota_helper temp) temp

	
