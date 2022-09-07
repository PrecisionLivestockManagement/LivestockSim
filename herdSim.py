import numpy as np

#

n = 100
BW = np.random.normal(50, 10, n)
DWG = np.random.normal(0.8, 0.4, n)
# GP = np.random.uniform(0.1, 0.9, n)


d = range(0, 10)

for i in d:
    print(BW + DWG * i)


