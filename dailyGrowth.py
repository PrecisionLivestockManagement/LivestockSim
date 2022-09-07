import numpy as np
from tabulate import tabulate as tb

# BW = np.random.choice(range(40, 61))
# GP = np.random.choice(np.arange(0.1, 1, 0.1))
ID = 1001
BW = 50
GP = 0.5

Month = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

FA = {"Jan": 2, "Feb": 3, "Mar": 4, "Apr": 7, "May": 8, "Jun": 5, "Jul": 6, "Aug": 9, "Sep": 10, "Oct": 12, "Nov": 11, "Dec": 1}

# Assuming an animal is born in 1 Jan 
# Daily wt gain for each month
DWG_Jan = BW/50 + GP + FA["Jan"]/6
DWG_Feb = BW/50 + GP + FA["Feb"]/6
DWG_Mar = BW/50 + GP + FA["Mar"]/6
DWG_Apr = BW/50 + GP + FA["Apr"]/6
DWG_May = BW/50 + GP + FA["May"]/6
DWG_Jun = BW/50 + GP + FA["Jun"]/6
DWG_Jul = BW/50 + GP + FA["Jul"]/6
DWG_Aug = BW/50 + GP + FA["Aug"]/6
DWG_Sep = BW/50 + GP + FA["Sep"]/6
DWG_Oct = BW/50 + GP + FA["Oct"]/6
DWG_Nov = BW/50 + GP + FA["Nov"]/6
DWG_Dec = BW/50 + GP + FA["Dec"]/6

######### animal weights calculation for the month of January ###########
#########################################################################
WT_Jan = []
for i in range(1, 32): 
    CALC_Jan = BW + DWG_Jan * (i - 1)
    WT_Jan.append(round(CALC_Jan, 1))
    
FWT_Jan = WT_Jan[30]

######### animal weights calculation for the month of February ##########
#########################################################################
WT_Feb = []
for i in range(1, 29): 
    CALC_Feb = FWT_Jan + DWG_Feb * i
    WT_Feb.append(round(CALC_Feb, 1))

FWT_Feb = WT_Feb[27]

######### animal weights calculation for the month of March ###########
#######################################################################
WT_Mar = []
for i in range(1, 32): 
    CALC_Mar = FWT_Feb + DWG_Mar * i
    WT_Mar.append(round(CALC_Mar, 1))

FWT_Mar = WT_Mar[30]

######### animal weights calculation for the month of April ###########
#######################################################################
WT_Apr = []
for i in range(1, 31): 
    CALC_Apr = FWT_Mar + DWG_Apr * i
    WT_Apr.append(round(CALC_Apr, 1))

FWT_Apr = WT_Apr[29]

######### animal weights calculation for the month of May ###########
#####################################################################
WT_May = []
for i in range(1, 32): 
    CALC_May = FWT_Apr + DWG_May * i
    WT_May.append(round(CALC_May, 1))

FWT_May = WT_May[30]

######### animal weights calculation for the month of June ###########
######################################################################
WT_Jun = []
for i in range(1, 31): 
    CALC_Jun = FWT_May + DWG_Jun * i
    WT_Jun.append(round(CALC_Jun, 1))

FWT_Jun = WT_Jun[29]

######### animal weights calculation for the month of July ###########
######################################################################
WT_Jul = []
for i in range(1, 32): 
    CALC_Jul = FWT_Jun + DWG_Jul * i
    WT_Jul.append(round(CALC_Jul, 1))

FWT_Jul = WT_Jul[30]

######### animal weights calculation for the month of August ###########
########################################################################
WT_Aug = []
for i in range(1, 32): 
    CALC_Aug = FWT_Jul + DWG_Aug * i
    WT_Aug.append(round(CALC_Aug, 1))

FWT_Aug = WT_Aug[30]

######### animal weights calculation for the month of September ###########
###########################################################################
WT_Sep = []
for i in range(1, 31): 
    CALC_Sep = FWT_Aug + DWG_Sep * i
    WT_Sep.append(round(CALC_Sep, 1))

FWT_Sep = WT_Sep[29]

######### animal weights calculation for the month of October ###########
#########################################################################
WT_Oct = []
for i in range(1, 32): 
    CALC_Oct = FWT_Sep + DWG_Oct * i
    WT_Oct.append(round(CALC_Oct, 1))

FWT_Oct = WT_Oct[30]

######### animal weights calculation for the month of November ###########
##########################################################################
WT_Nov = []
for i in range(1, 31): 
    CALC_Nov = FWT_Oct + DWG_Nov * i
    WT_Nov.append(round(CALC_Nov, 1))

FWT_Nov = WT_Nov[29]

######### animal weights calculation for the month of December ###########
##########################################################################
WT_Dec = []
for i in range(1, 32): 
    CALC_Dec = FWT_Nov + DWG_Dec * i
    WT_Dec.append(round(CALC_Dec, 1))

FWT_Dec = WT_Dec[29]

# tabulation of weight data
table = {'January': WT_Jan, "February": WT_Feb, "March": WT_Mar, "April": WT_Apr, "May": WT_May, "June": WT_Jun, "July": WT_Jul, "August": WT_Aug, "September": WT_Sep, "October": WT_Oct, "November": WT_Nov, "December": WT_Dec}

print ("Daily weights of an animal with id:", ID, "\n")
print(tb(table, headers='keys'))