import numpy as np
import matplotlib.pyplot as plt

# basic functions
def elementAt(it, n):
    return next((x for i,x in enumerate(it) if i==n), None)

# constants
C = 9.96 * np.power(10, 6)
emissivity = 0.62
boltz = 5.67 * np.power(10.0,-8)
S = 1367
albedo = 0.3

# time series definition
t0 = 0
T = 100
n = 100
y0 = 20

c1 = 1 / (4*C)
c2 = boltz * emissivity / C
delta_t = (T - t0) / n
def f(y, t):
    return c1 * S * (1 - albedo) - c2 * np.power(y, 4)
def t(i): # t is not implemented as a generator because every step just adds delta_t
    return t0 + delta_t * i
def y():
    last_y = y0
    for k in range(0, n):
        next_y = last_y + delta_t * f(last_y, t(k))
        yield next_y
        last_y = next_y

# create arrays from functions
y_cache = np.empty(n)
for i, el in enumerate(y()): y_cache[i] = el
t_cache = []
for i in range(0, n): t_cache.append(t(i))

# print arrays
for i in range(0, n - 1):
    print(t_cache[i], y_cache[i])

# plot arrays
plt.plot(t_cache, y_cache)
plt.title("Energy Balance Model")
plt.ylabel("Temperature in C")
plt.xlabel("Time in days since first measurement")
plt.ticklabel_format(useOffset=False)
plt.show()