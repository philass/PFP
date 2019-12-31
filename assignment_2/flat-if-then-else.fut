-- Flattening If-Then-Else nested inside a map
-- ==
-- compiled input { [false,true,false,true]
--                  [3,4,2,1]
--                  [1,2,3,4,5,6,7,8,9,10]
-- }
-- output { [3,4,2,1] [2,4,6,5,6,7,8,16,18,11] }
--

let sgmscan 't [n] (op: t->t->t) (ne: t) (flg : [n]i32) (arr : [n]t) : [n]t =
  let flgs_vals = 
    scan ( \ (f1, x1) (f2,x2) -> 
            let f = f1 | f2 in
            if f2 != 0 then (f, x2)
            else (f, op x1 x2) )
         (0,ne) (zip flg arr)
  let (_, vals) = unzip flgs_vals
  in vals

let scanExc 't [n] (op: t->t->t) (ne: t) (arr : [n]t) : [n]t =
    scan op ne <| map (\i -> if i>0 then unsafe arr[i-1] else ne) (iota n)

let mkFlagArray 't [m] 
            (aoa_shp: [m]i32) (zero: t)       --aoa_shp=[0,3,1,0,4,2,0]
            (aoa_val: [m]t  ) : []t = unsafe  --aoa_val=[1,1,1,1,1,1,1]
  let shp_rot = map (\i->if i==0 then 0       --shp_rot=[0,0,3,1,0,4,2]
                         else aoa_shp[i-1]
                    ) (iota m)
  let shp_scn = scan (+) 0 shp_rot            --shp_scn=[0,0,3,4,4,8,10]
  let aoa_len = shp_scn[m-1]+aoa_shp[m-1]     --aoa_len= 10
  let shp_ind = map2 (\shp ind ->             --shp_ind= 
                       if shp==0 then -1      --  [-1,0,3,-1,4,8,-1]
                       else ind               --scatter
                     ) aoa_shp shp_scn        --   [0,0,0,0,0,0,0,0,0,0]
  in scatter (replicate aoa_len zero)         --   [-1,0,3,-1,4,8,-1]
             shp_ind aoa_val                  --   [1,1,1,1,1,1,1]
                                              -- res = [1,0,0,1,1,0,0,0,1,0] 


let partition2 [n] 't (conds: [n]bool) (dummy: t) (arr: [n]t) : (i32, [n]t) =
  let tflgs = map (\ c -> if c then 1 else 0) conds
  let fflgs = map (\ b -> 1 - b) tflgs

  let indsT = scan (+) 0 tflgs
  let tmp   = scan (+) 0 fflgs
  let lst   = if n > 0 then indsT[n-1] else -1
  let indsF = map (+lst) tmp

  let inds  = map3 (\ c indT indF -> if c then indT-1 else indF-1) conds indsT indsF

  let fltarr= scatter (replicate n dummy) inds arr
  in  (lst, fltarr)

let mkII2 [m] (shp: [m]i32) : []i32 =
    let n = reduce (+) 0i32 shp
    let F = mkFlagArray shp 0 (replicate m 1)
    in  sgmscan (+) 0 F (replicate n 1) |> map (\x->x-1)

-- Weekly 2, Exercise 2:
-- The function bellow should be the flatten version of:
--     map (\b xs -> if b then map f xs
--                        else map g xs
--         ) bs xss
-- where:
--   `bs` is a 1d array of booleans
--   `xss` is a 2d irregular array of shape `S1_xss` and flat data `D_xss`;
--         the shape is an array of size `m` and the data is an array of size `n`
--   the result is a tuple representing an irregular array:
--       result's shape
--       result's flat data
-- Please take a look at the Rule (8) of Flattening 
-- (slides 34 and 35 of L4-irreg-flattening.pdf) 
-- and adapt the code from there.
--
-- The task is of course to replace the dummy implementation below
-- that just returns the input array with the code that performs the
-- flattening. 
-- (You may of course use the helper functions provided in this file.)
--
--let flatIf [n][m] (f: i32 -> i32) (g: i32->i32) (bs: [m]bool) (S1_xss: [m]i32, D_xss: [n]i32) : ([]i32, []i32) =
--  let S_1_res = S1_xss
--  let D_res = D_xss
--  in  (S_1_res, D_res)

let flatIf [n][m] (f: i32 -> i32) (g: i32->i32) (bs: [m]bool) (S1_xss: [m]i32, D_xss: [n]i32) : ([]i32, []i32) =
  let (spl, iinds) = partition2 (\i -> bs[i]) (iota (length bs))
  let (S_then, S_else) = split spl (map (\ii -> S1_xss[ii]) inds)
  let F = mkFlagArray shp 0 (replicate m 1)
  let II1 = sgmscan (+) 0 F F |> map (-1)
  let mask_xss = map (\sgmind -> bs[sgmind]) II1
  let (brk, Dp_xss) = partition2 mask_xss D_xss
  let (D_xss_then, D_xss_else) = split brk Dp_xssp
  let (S1_res_then, D_res_then) = (S_then, map f D_xss_then)
  let (S1_res_else, D_res_else) = (S_else, map f D_xss_else)
  let S1_res_p = concat S1_res_then S1_res_else
  let S1_res = scatter (replicate (length bs) 0) iinds S1_res_p
  let B_1_res = scanExc (+) 0 S1_res
  let F_p_res = mkFlagArray S1_res_p 0 (map (+1) iinds)
  let II1_p = sgmscan (+) F_p_res F_p_res |> map (-1)
  let II1_res_then = mkII2 S1_res_then
  let II2_res_else = mkII2 S1_res_else
  -- COULD BE WRONG ^^
  --need to get II1_res_then
  --need to get II2_res_then
  let II2_p = concat II2_res_then II2_res_else 
  let sinds_res  = map2 (\sgm iin -> B_1_res[sgm] + iin) II1_p II2_p
  let flen_res = length F_p_res
  --need to get flen_res
  let D_res = scatter (replicate flen_res 0) sinds_res (concat D_res_then D_res_else)
  in (S1_res, D_res)

 





--Helper Functions provided
-- partition2
-- MkII2
-- sgmscan
-- scanExc
-- mkFlagArray


-- echo "[false,true,false,true] [3,4,2,1] [1,2,3,4,5,6,7,8,9,10]" | ./flat-if-then-else
let main [n][m] (bs: [m]bool) (S1_xss: [m]i32) (D_xss: [n]i32) = 
    flatIf (\x->x+1i32) (\x->2i32*x) bs (S1_xss, D_xss)
