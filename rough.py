
BW = 50
GP = 0.5
FA = int
month = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

# DWG = BW/50 + GP + FA/6

for i in month:
    if i == month[0]:
        FA = 2
    elif i == month[1]:
        FA = 3
    else:
        FA = 4
        
    DWG = BW/50 + GP + FA/6
    print(DWG)   
        
        
# if month[0]: 
#     FA =2
# if month[1]: 
#     FA =3
# if month[2]: 
#     FA =4
# if month[3]: 
#     FA =7
# if month[4]: 
#     FA =8
# if month[5]: 
#     FA =5
# if month[6]: 
#     FA =6
# if month[7]: 
#     FA =9
# if month[8]: 
#     FA =10
# if month[9]: 
#     FA =12
# if month[10]: 
#     FA =11
# if month[11]: 
#     FA =1