library(DMMongoDB)
library(dplyr)
library(zoo)
library(tidyverse)

############ getting all cattle at CQIRP ##################
all_cows_CQIRP <- get_cattle(property = "CQIRP")
daily_wts <- get_dailywts(property = "CQIRP")
static_wts <- get_staticwts(property = "CQIRP")


# plotting daily wts; date vs weights ####
ggplot(daily_wts, aes(x = Date, y = Weight, color = RFID)) +
  geom_line()

# plotting WOW data
ggplot(all_cows_WOW, aes(x = datetime, y = Wt, color = RFID)) +
  geom_line()

# plotting static weights vs date
ggplot(static_wts, aes(x = Date, y = Weight, color = RFID)) +
  geom_line()


# # function to calculate the rolling average of weights
# roll_avg <- function(wts){
#   rollmean(wts, k =7, fill = NA)
# }
# roll_avg_wt <- roll_avg(all_cows_wt$Weight)
# all_cows_avg_wt <- cbind(all_cows_wt, roll_avg_wt )
# 
# # plotting WOW date vs weights taking rolling average of 7 days
# ggplot(data = all_cows_avg_wt, mapping = aes(x = Date, y = roll_avg_wt, color = RFID)) + geom_line()



############# Frequency plots ############

ggplot(daily_wts, aes(x = Date, color = RFID)) +
  geom_histogram(fill = 'white')

# plaotting in facet
ggplot(daily_wts, mapping = aes(Date)) +
  geom_histogram() +
  facet_wrap(~ RFID)
########  outliers removal for wow data####################################
# removing front foot weights
rm_ff <- subset(all_cows_WOW, Location != "29.FrontFoot")

# removing 0 weights
rm_zero <- subset(rm_ff, Wt != 0)

# replacing date-time with dates only format
rm_zero$Date <- c(as.Date(rm_zero$datetime))

#grouping data set for individual animal
cleaned_data <- rm_zero[, -c(4, 5)]

ggplot(cleaned_data, aes(x = datetime, y = Wt, color = RFID)) +
  geom_line()

########  outliers removal for daily wts data ####################################
# removing front foot weights
rm_ff1 <- subset(daily_wts, Location != "29.FrontFoot")

# removing 0 weights
rm_zero1 <- subset(rm_ff1, Weight != 0)

# replacing date-time with dates only format
rm_zero1$Date <- c(as.Date(rm_zero1$Date))

#grouping data set for individual animal
cleaned_data1 <- rm_zero1[, -c(4, 5)]

ggplot(cleaned_data1, aes(x = Date, y = Weight, color = RFID)) +
  geom_line()


###### for cow 1
cow1_wts <- subset(cleaned_data, RFID == "982 123779046918")
ggplot(cow1_wts, aes(x = Date, y = Weight)) +
  geom_point()


daily_mean <- c()

cow1_daily_wts <- aggregate(Weight ~ Date, data = cow1_wts, FUN = mean)
ggplot(cow1_daily_wts, aes(x = Date, y = Weight)) +
  geom_point()


wow_CQIRP <- get_wowdata(property = "CQIRP")

wkly_wts <- get_weeklywts(RFID = '982 123779046918')


#### summary statistics #####

