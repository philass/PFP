import matplotlib.pyplot as plt
import math

f = open('pyplot2.txt', 'r')
x = f.readlines()
f.close()

x = [float(v) for v in x]

ex1 = x[:14]
ex3 = x[14:]

plt.semilogx(ex1[0::2], ex1[1::2])
plt.semilogx(ex3[0::2], ex3[1::2])
plt.title("Process_idx run time")
plt.ylabel("Run Time (µs)")
plt.xlabel("Size")
plt.legend(["OpenCL", "C"])
plt.show()
print(ex1)
print(ex3)

