-- ==
-- input @ data_16_131072
-- input @ data_32_32768
-- input @ data_64_8192
-- input @ data_128_2048
-- input @ data_256_512
-- input @ data_512_128
-- input @ data_1024_32


let gaussian_elimination [n] [m] (A: [n][m]f32): [n][m]f32 =
  loop A for i < n do
    let v1 = A[0,i]
    let elem k j = let x = unsafe (A[0,j] / v1) in
                   if k < n-1  -- Ap case
                   then unsafe (A[k+1,j] - A[k+1,i] * x)
                   else x      -- irow case
    in tabulate_2d n m elem


let matrix_inverse [n] (A: [n][n]f32): [n][n]f32 = 
  let A_aug = unsafe tabulate_2d n (2 * n) (\i j -> if j < n then A[i, j] else 
                                                                (if i == (j - n) then 1 else 0))
  let R_aug = gaussian_elimination A_aug
  in R_aug[0:n,n:n * 2]



let main [k][n] (As: [k][n][n]f32) : [k][n][n]f32 = map matrix_inverse As





