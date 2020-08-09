from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import pandas as pd
import seaborn as sns
import os

plotData1 = []
plotData2 = []
plotData3 = []

with open("outfile3D.txt") as tf: 
    Lines = tf.readlines()
    i = 0
    for line in Lines:
        split = line.split()
        nums = []
        if (i-0) % 32 == 0:
            j = 0
            for strNum in split:
                plotData1.append(i)
                plotData2.append(j)
                plotData3.append(float(strNum))
                j += 1
        i += 1

        if i == 1600:
            break

# Make the plot
fig = plt.figure()
ax = fig.gca(projection='3d')
ax.plot_trisurf(plotData1, plotData2, plotData3, cmap=cm.get_cmap("Spectral"), linewidth=0.2)
plt.show()