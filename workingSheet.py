import numpy as np
from tabulate import tabulate as tb

# BW = np.random.normal(40, 10)
# GP = np.random.uniform(0.1, 1)

ID = 1001
LWT = 0

BW = input("Birth weight:")
BW = float(BW)

Month = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

# forage availability index for each month; dependa on forage quantity
FAI = {"Jan": 1, "Feb": 1, "Mar": 0.9, "Apr": 0.8, "May": 0.6, "Jun": 0.7, "Jul": 0.6, "Aug": 0.5, "Sep": 0.5, "Oct": 0.7, "Nov": 0.8, "Dec": 0.9}

# Metabolic energy present in feed (Mcal/kg); depends on forage quality
ME = {"Jan": 3, "Feb": 3, "Mar": 3, "Apr": 2, "May": 1, "Jun": 2, "Jul": 1, "Aug": 1, "Sep": 1, "Oct": 2, "Nov": 2, "Dec": 3}

# Net energy in forage for maintenance
NEm = 1.37 * ME - 0.138 * ME**2 + 0.0105 * ME**3 -1.12

# Net energy in forage for growth
NEg = 1.42 * ME - 0.174 * ME**2 + 0.0122 * ME**3 -1.65

# Maximum feed intake
MFI = LWT**0.75 * (0.1493 * NEm - 0.046 * NEm**2 - 0.0196)

# Actual feed intake
AFI = MFI * FAI

# Net energy available to animal for weight gain
NEG = (AFI - NEm) * NEg

# Liveweight gain 
DWG = 13.91 * NEG**0.9116 * LWT**-0.6837

###### calculation for January ##########
NEm_Jan = 1.37 * ME["Jan"] - 0.138 * ME["jan"]**2 + 0.0105 * ME["Jan"]**3 -1.12
NEg_Jan = 1.42 * ME["Jan"] - 0.174 * ME["Jan"]**2 + 0.0122 * ME["Jan"]**3 -1.65
MFI_Jan = LWT**0.75 * (0.1493 * NEm_Jan - 0.046 * NEm_Jan**2 - 0.0196)
AFI_Jan = MFI_Jan * FAI["Jan"]
NEG_Jan = (AFI_Jan - NEm_Jan) * NEg_Jan


LWT_Jan1 = 50
# Daily weight gain
DWG_Jan1 = 13.91 * NEG_Jan**0.9116 * LWT_Jan1**-0.6837


LWT_Jan2 = LWT_Jan1 + DWG_Jan1
DWG_Jan2 = 13.91 * NEG_Jan**0.9116 * LWT_Jan2**-0.6837


WT_Jan = [LWT_Jan1, LWT_Jan2]
print(WT_Jan)