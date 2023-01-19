library(DMMongoDB)
library(dplyr)
library(zoo)

# getting all cattle at CQIRP
all_cows_CQIRP <- get_cattle(property = "CQIRP")
all_cows_wt <- get_dailywts(all_cows_CQIRP)

# getting daily wts data for individual cows
RFIDS <- all_cows_CQIRP$RFID
cow_1 <- get_cattle(RFID = "982 123778981825")

cow_1_wts <- get_dailywts(cow_1)
cow_2_wts <- get_dailywts(RFID = "982 123778981826")
cow_3_wts <- get_dailywts(RFID = "982 123779046966")
cow_4_wts <- get_dailywts(RFID = "982 123779046918")
cow_5_wts <- get_dailywts(RFID = "982 123779046919")
cow_6_wts <- get_dailywts(RFID = "982 123779046952")
cow_7_wts <- get_dailywts(RFID = "982 123779046954")
cow_8_wts <- get_dailywts(RFID = "982 123779046927")
cow_9_wts <- get_dailywts(RFID = "982 123779046926")
cow_10_wts <- get_dailywts(RFID = "982 123779046963")
cow_11_wts <- get_dailywts(RFID = "982 123779046921")
cow_12_wts <- get_dailywts(RFID = "982 123779046958")



# plotting WOW date vs weights
plot(cow_1_wts$Date, cow_1_wts$Weight, type="l", xlab = "Date", ylab = "weight")
lines(cow_2_wts$Date, cow_2_wts$Weight, type = "l", col = "red")
lines(cow_3_wts$Date, cow_3_wts$Weight, type = "l", col = "blue")
lines(cow_4_wts$Date, cow_4_wts$Weight, type = "l", col = "green")
lines(cow_5_wts$Date, cow_5_wts$Weight, type = "l", col = "purple")
lines(cow_6_wts$Date, cow_6_wts$Weight, type = "l", col = "grey")
lines(cow_7_wts$Date, cow_7_wts$Weight, type = "l", col = "violet")
lines(cow_8_wts$Date, cow_8_wts$Weight, type = "l", col = "yellow")
lines(cow_9_wts$Date, cow_9_wts$Weight, type = "l", col = "#4B0082")
lines(cow_10_wts$Date, cow_10_wts$Weight, type = "l", col = "orange")
lines(cow_11_wts$Date, cow_11_wts$Weight, type = "l", col = "pink")
lines(cow_12_wts$Date, cow_12_wts$Weight, type = "l", col = "#ffffcc")


# function to calculate the rolling average of weights
roll_avg <- function(wts){
  rollmean(wts, k =7, fill = NA)
}

cow_1_avg <- roll_avg(cow_1_wts$Weight)
cow_2_avg <- roll_avg(cow_2_wts$Weight)
cow_3_avg <- roll_avg(cow_3_wts$Weight)
cow_4_avg <- roll_avg(cow_4_wts$Weight)
cow_5_avg <- roll_avg(cow_5_wts$Weight)
cow_6_avg <- roll_avg(cow_6_wts$Weight)
cow_7_avg <- roll_avg(cow_7_wts$Weight)
cow_8_avg <- roll_avg(cow_8_wts$Weight)
cow_9_avg <- roll_avg(cow_9_wts$Weight)
cow_10_avg <- roll_avg(cow_10_wts$Weight)
cow_11_avg <- roll_avg(cow_11_wts$Weight)
cow_12_avg <- roll_avg(cow_12_wts$Weight)

plot(cow_1_wts$Date, cow_1_avg, type="l", xlab = "Date", ylab = "weight")
lines(cow_2_wts$Date, cow_2_avg, type = "l", col = "red")
lines(cow_3_wts$Date, cow_3_avg, type = "l", col = "blue")
lines(cow_4_wts$Date, cow_4_avg, type = "l", col = "green")
lines(cow_5_wts$Date, cow_5_avg, type = "l", col = "purple")
lines(cow_6_wts$Date, cow_6_avg, type = "l", col = "#990000")
lines(cow_7_wts$Date, cow_7_avg, type = "l", col = "violet")
lines(cow_8_wts$Date, cow_8_avg, type = "l", col = "yellow")
lines(cow_9_wts$Date, cow_9_avg, type = "l", col = "#4B0082")
lines(cow_10_wts$Date, cow_10_avg, type = "l", col = "orange")
lines(cow_11_wts$Date, cow_11_avg, type = "l", col = "pink")
lines(cow_12_wts$Date, cow_12_avg, type = "l", col = "#666633")

# legend setup
legends <- c("Cow 1", "Cow 2", "Cow 3", "Cow 4", "Cow 5", "Cow 6", "Cow 7", "Cow 8", "Cow 9", "Cow 10", "Cow 11", "Cow 12")
colors <- c("black", "red", "blue", "green", "purple", "#990000", "violet", "yellow", "#4B0082", "orange", "pink", "#666633")

legend("bottomright", legend = legends, fill = colors)

# 
# plot(cow_1_wts$Date, cow_1_avg, type = "l")
# lines (cow_2_wts$Date, cow_2_avg, col = "red")
