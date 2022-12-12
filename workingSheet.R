# import numpy as np
# from tabulate import tabulate as tb
# 
# BW = 50
# # for the month of January
# ME_Jan = 4 # Mcal per Kg
# NEm_Jan = 1.37 * ME_Jan -0.138 * pow(ME_Jan, 2) + 0.0105 * pow(ME_Jan, 3) - 1.12
# NEg_Jan = 1.42 * ME_Jan -0.174 * pow(ME_Jan, 2) + 0.0122 *pow(ME_Jan, 3) - 1.65
# FAI_Jan = 1
# 
# # for Jan 1
# LWT_Jan1 = 50
# MFI_Jan1 = pow(LWT_Jan1, 0.75) * (0.1493 * NEm_Jan - 0.046 * NEm_Jan**2 - 0.0196)
# 
# AFI_Jan1 = MFI_Jan1 * FAI_Jan
# NEG_Jan1 = (AFI_Jan1 - NEm_Jan) * NEg_Jan
# LWG_Jan1 = 13.91 * pow(NEG_Jan1, 0.9116) * pow(LWT_Jan1, -0.6837)
# 
# # for jan 2
# LWT_Jan2 = LWT_Jan1 + LWG_Jan1
# MFI_Jan2 = pow(LWT_Jan2, 0.75) * (0.1493 * NEm_Jan - 0.046 * NEm_Jan**2 - 0.0196)
# AFI_Jan2 = MFI_Jan2 * FAI_Jan
# NEG_Jan2 = (AFI_Jan2 - NEm_Jan) * NEg_Jan
# LWG_Jan2 = 13.91 * pow(NEG_Jan2, 0.9116) * pow(LWT_Jan2, -0.6837)
# 
# # for jan 3
# LWT_Jan3 = LWT_Jan2 + LWG_Jan2
# MFI_Jan3 = pow(LWT_Jan3, 0.75) * (0.1493 * NEm_Jan - 0.046 * NEm_Jan**2 - 0.0196)
# AFI_Jan3 = MFI_Jan3 * FAI_Jan
# NEG_Jan3 = (AFI_Jan3 - NEm_Jan) * NEg_Jan
# LWG_Jan3 = 13.91 * pow(NEG_Jan3, 0.9116) * pow(LWT_Jan3, -0.6837)
# 
# # for jan 4
# LWT_Jan4 = LWT_Jan3 + LWG_Jan3
# 
# LWT_Jan = [LWT_Jan1, LWG_Jan2, LWT_Jan3, LWT_Jan4]
# print(LWT_Jan)


