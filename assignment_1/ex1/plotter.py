import matplotlib.pyplot as plt
import math

f = open('pyplot.txt', 'r')
x = f.readlines()
f.close()

x = [float(v) for v in x]

ex1 = x[:14]
ex3 = x[14:]
log1 = [math.log(val, 10) for val in ex1]
log3 = [math.log(val, 10) for val in ex3]

plt.plot(log1[0::2], ex1[1::2])
plt.plot(log3[0::2], ex3[1::2])
plt.title("Process run time")
plt.ylabel("Run Time (µs)")
plt.xlabel("Size")
plt.legend(["OpenCL", "C"])
plt.show()
print(ex1)
print(ex3)

