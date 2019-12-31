#!/usr/bin/env python3
import os

def gen_statement(n, m):
  return "futhark dataset -b --f32-bounds=-10000:10000 -g [" + str(m) + "][" + str(n) + "][" + str(n) + "]f32 > data_" + str(n) + "_" + str(m)



#we want mn^2 = K as we very m and n. Let K = 2^25. Then we can rewrite as
# (2^M)(2^N)^2 = 2 ^25
# M  + 2 * N = 25
# M = 25 - 2N
#THEN 


N = [i for i in range(4, 11)]
M = [25 - (2 * i) for i in N]

print(N)
print(M)
for i in range(7):
  os.system(gen_statement(2**N[i], 2**M[i]))
