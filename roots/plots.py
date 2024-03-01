import matplotlib.pyplot as plt
import numpy as np
import matplotlib as mpl
mpl.rcParams.update(mpl.rcParamsDefault)
plt.style.use(['science','grid','muted'])



"""
This program is used for plot the results from the ODE Fortran Solver

"""
# Import the data
data = np.loadtxt('./data/ivp_sol.dat', delimiter = None)

t = data[:,0]
y = data[:,1]
v = data[:,2]

fig, ax = plt.subplots() 

ax.plot(t, y, label = 'y')
ax.plot(t, v, label = 'v') 

ax.set_xlabel('$t$')
ax.set_ylabel('$y$')
ax.legend(fontsize=7)



fig.savefig('shooting.pdf')
