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
    yield (t0, y0)
    for k in range(1, n):
        yk_next = []
        curT = t(k, t0, delta_t)
        phiout = phi(curT, yk, f, curT, delta_t, p)
        for i in range(0, yk.__len__()):
            yk_next.append(yk[i] + delta_t * phiout[i])
        yield (curT, yk_next)
        yk = yk_next

# Change this to swap algs
algId = 2

if algId == 0:
    # Energy Balance Model
    c1 = 1 / (4*const.C) * const.S * (1 - const.albedo)
    c2 = const.boltz * const.emissivity / const.C
    loopRe = timeLoop(
        phi=lambda t, yk, f, tk, delta_t, p: f(yk, t, p), 
        f=lambda y, t, p: [p["c1"] + p["c2"] * np.power(y[0], 4)], 
        t0=0, T=50000000, y0=[20], p={
            "c1": 1 / (4*const.C) * const.S * (1 - const.albedo), 
            "c2": -const.boltz * const.emissivity / const.C}, n=1000)
    ys = list(loopRe)
    plotTitle = "Energy Balance Model"
    plotXLabel = "Time in days since first measurement"
    plotYLabel = "Temperature in C"
if algId == 1:
    # predator-prey model
    loopRe = timeLoop(phi=lambda t, yk, f, tk, delta_t, p: f(yk, t, p), 
                      f=lambda y, t, p: [y[0] * (p["alpha"] - p["beta"] * y[1] - p["half_life"] * y[0]),
                                         y[1] * (p["delta"] * y[0] - p["gamma"] - p["mu"] * y[1])], 
                      t0=0, T=50, y0=[1, 2], p={
                        "alpha": 1.2,
                        "beta": 1.2,
                        "gamma": 1.1,
                        "delta": 2.1,
                        "half_life": 0.1,
                        "mu": 0.1,
                      }, n=2000)
    ys = list(loopRe)
    plotTitle = "Predator Prey Model"
    plotXLabel = "Time in days since first measurement"
    plotYLabel = "Population Count"
if algId == 2:
    # predator-prey model 2: improved Euler
    def improvedEuler(t, yk, f, tk, delta_t, p):
        halfStep = list(map(lambda x: delta_t * 0.5 * x, f(yk, t, p)))
        yHalf = []
        for i in range(0, yk.__len__()):
            yHalf.append(yk[i] + halfStep[i])
        return f(yHalf, tk + delta_t / 2, p)

    loopRe = timeLoop(phi=lambda t, yk, f, tk, delta_t, p: improvedEuler(t, yk, f, tk, delta_t, p), 
                      f=lambda y, t, p: [y[0] * (p["alpha"] - p["beta"] * y[1] - p["half_life"] * y[0]),
                                         y[1] * (p["delta"] * y[0] - p["gamma"] - p["mu"] * y[1])], 
                      t0=0, T=50, y0=[1, 2], p={
                        "alpha": 1.2,
                        "beta": 1.2,
                        "gamma": 1.1,
                        "delta": 2.1,
                        "half_life": 0.1,
                        "mu": 0.1,
                      }, n=2000)
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