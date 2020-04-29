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
        curT = t(k, t0, delta_t)
        phiout = phi(curT, yk, f, delta_t)
        for i in range(0, yk.__len__()):
            yk_next.append(yk[i] + delta_t * phiout[i])
        yield (curT, yk_next)
        yk = yk_next

# Change this to swap algs
algId = 1

if algId == 0:
    # Energy Balance Model
    c1 = 1 / (4*const.C)
    c2 = const.boltz * const.emissivity / const.C
    loopRe = timeLoop(
        phi=lambda t, yk, f, delta_t: f(yk, t), 
        f=lambda y, t: [c1 * const.S * (1 - const.albedo) - c2 * np.power(y[0], 4)], 
        t0=0, T=50000000, y0=[20], p=[], n=1000)
    ys = list(loopRe)
    plotTitle = "Energy Balance Model"
    plotXLabel = "Time in days since first measurement"
    plotYLabel = "Temperature in C"
if algId == 1:
    # predator-prey model
    alpha = 1.2
    beta = 1.2
    gamma = 1.1
    delta = 2.1
    half_life = 0.1
    mu = 0.1
    loopRe = timeLoop(phi=lambda t, yk, f, delta_t: f(yk, t), 
                      f=lambda y, t: [y[0] * (alpha - beta * y[1] - half_life * y[0]),
                                      y[1] * (delta * y[0] - gamma - mu * y[1])], 
                      t0=0, T=50, y0=[1, 2], p=[], n=2000)
    ys = list(loopRe)
    plotTitle = "Predator Prey Model"
    plotXLabel = "Time in days since first measurement"
    plotYLabel = "Population Count"
if algId == 2:
    # predator-prey model 2: electric bogaloo
    alpha = 1.2
    beta = 1.2
    gamma = 1.1
    delta = 2.1
    half_life = 0.1
    mu = 0.1
    loopRe = timeLoop(phi=lambda t, yk, f, delta_t: list(map(lambda x: x * 2, f(yk, t))), 
                      f=lambda y, t: [y[0] * (alpha - beta * y[1] - half_life * y[0]),
                                      y[1] * (delta * y[0] - gamma - mu * y[1])], 
                      t0=0, T=50, y0=[1, 2], p=[], n=1000)
    ys = list(loopRe)
    plotTitle = "Predator Prey Model 2"
    plotXLabel = "Time in days since first measurement"
    plotYLabel = "Population Count"

# print output to console
for i in range(0, ys.__len__()):
    print(str(ys[i][0]) + ": " + str(ys[i][1]))

# plot arrays
for i in range(0, ys[0][1].__len__()):
    plt.plot(list(map(lambda x: x[0], ys)), list(map(lambda x: x[1][i], ys)))
plt.title(plotTitle)
plt.xlabel(plotXLabel)
plt.ylabel(plotYLabel)
plt.ticklabel_format(useOffset=False)
plt.show()