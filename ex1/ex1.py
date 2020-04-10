import numpy as np
import matplotlib.pyplot as plt
import constants as const
import util as util

printArrays = False

# time series definition
t0 = 0
T = 100
n = 100
y0 = 20
c1 = 1 / (4*const.C)
c2 = const.boltz * const.emissivity / const.C
delta_t = (T - t0) / n
def f(y, t):
    return c1 * const.S * (1 - const.albedo) - c2 * np.power(y, 4)
def t(i): # t is not implemented as a generator because every step just adds delta_t
    return t0 + delta_t * i
def y():
    last_y = y0
    for k in range(0, n):
        next_y = last_y + delta_t * f(last_y, t(k))
        yield next_y
        last_y = next_y

# create arrays from functions
y_cache = util.createArray(y(), lambda x: x)
t_cache = util.createArray(range(0, n), lambda x: t(x))

# print arrays
if printArrays:
    for i in range(0, n - 1):
        print(t_cache[i], y_cache[i])

# plot arrays
plt.plot(t_cache, y_cache)
plt.title("Energy Balance Model")
plt.ylabel("Temperature in C")
plt.xlabel("Time in days since first measurement")
plt.ticklabel_format(useOffset=False)
plt.show()