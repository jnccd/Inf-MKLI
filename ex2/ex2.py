import numpy as np
import matplotlib.pyplot as plt
import constants as const
import util as util

def t(i, t0, delta_t):
    return t0 + delta_t * i

# time series definition
def timeLoop(phi, f, t0, T, y0, p, n):
    yk = y0
    delta_t = (T - t0) / n
    for k in range(0, n):
        yk_next = []
        phiout = phi(t(k, t0, delta_t), yk, f, delta_t)
        for i in range(0, yk.__len__()):
            yk_next.append(yk[i] + delta_t * phiout[i])
        yield yk_next
        yk = yk_next

# Change this to swap algs
algId = 1

if algId == 0:
    # Energy Balance Model
    n = 100
    t0 = 0
    T = 50000000
    y0 = [20]
    delta_t = (T - t0) / n
    ts = list(map(lambda x: t(x, t0, delta_t), range(0, n + 1)))
    def phi(t, yk, f, delta_t):
        return f(yk, t)
    c1 = 1 / (4*const.C)
    c2 = const.boltz * const.emissivity / const.C
    def f(y, t):
        return [c1 * const.S * (1 - const.albedo) - c2 * np.power(y[0], 4)]
    loopRe = timeLoop(phi, f, t0, T, y0, [], n)
    ys = list(loopRe)
if algId == 1:
    # predator-prey model
    n = 100
    t0 = 0
    T = 100
    y0 = [1, 2]
    delta_t = (T - t0) / n
    ts = list(map(lambda x: t(x, t0, delta_t), range(0, n)))
    alpha = 0.2
    beta = 0.2
    gamma = 0.1
    delta = 0.1
    half_life = 0.1
    mü = 0.1
    loopRe = timeLoop(lambda t, yk, f, delta_t: f(yk, t), 
                      lambda y, t: [y[0] * (alpha - beta * y[1] - half_life * y[0]),
                                    y[1] * (delta * y[0] - gamma - mü * y[1])], 
                      t0, T, y0, [], n)
    ys = list(loopRe)
if algId == 2:
    # predator-prey model 2: electric bogaloo
    n = 100
    t0 = 0
    T = 100
    y0 = [1, 2]
    delta_t = (T - t0) / n
    ts = list(map(lambda x: t(x, t0, delta_t), range(0, n)))
    alpha = 0.2
    beta = 0.2
    gamma = 0.1
    delta = 0.1
    half_life = 0.1
    mü = 0.1
    loopRe = timeLoop(lambda t, yk, f, delta_t: np.power(f(yk, t), 2), 
                      lambda y, t: [y[0] * (alpha - beta * y[1] - half_life * y[0]),
                                    y[1] * (delta * y[0] - gamma - mü * y[1])], 
                      t0, T, y0, [], n)
    ys = list(loopRe)

# print output to console
for i in range(0, ts.__len__()):
    print(str(ts[i]) + ": " + str(ys[i]))

# plot arrays
for i in range(0, ys[0].__len__()):
    plt.plot(ts, list(map(lambda x: x[i], ys)))
plt.title("Energy Balance Model")
plt.ylabel("Temperature in C")
plt.xlabel("Time in days since first measurement")
plt.ticklabel_format(useOffset=False)
plt.show()