library(devtools)
install_github("PrecisionLivestockManagement/DMMongoDB")
library(DMMongoDB)

#Ensure you have set up your username and password using dmaccess(“username”)


#To view all DMMongoDB functions and their descriptions enter the below code and click "Index" down the bottom of the help page
## IMPORTANT: Please only use the get_ functions. All other functions have the potential to change data in the DataMuster database
?DMMongoDB

#Enter the code below to view the details of the cattle in the current CQIRP herd:
cows <- get_cattle(property = "CQIRP")

#To specify particular details of the cattle to be returned you can use the fields call in the get_cattle() function. The field names come from the database. 
# If there are any details of the cattle you require I can tell you what the field name is: :
cows <- get_cattle(property = "CQIRP", fields = c("RFID", "properties.Management", "properties.entryDate"))

#Enter the code below to retrieve the daily weights of the current CQIRP herd:
wts <- get_dailywts(RFID = cows$RFID)

#To specify a particular date range of daily weights you can use the start call in the get_dailywts() function:
#I have used the date "2022-04-20" being the entry date of the heifers to CQIRP, shown in the cows dataframe (two heifers were from the previous CQIRP herd) 
wts <- get_dailywts(RFID = cows$RFID, start = "2022-04-20")






