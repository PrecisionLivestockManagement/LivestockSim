library(DMMongoDB)
library(dplyr)
library(zoo)
library(tidyverse)

############ getting all cattle at CQIRP ##################
all_cows_CQIRP <- get_cattle(property = "Belmont")
all_cows_wt <- get_dailywts(all_cows_CQIRP)




# plotting WOW date vs weights
ggplot(data = all_cows_wt, mapping = aes(x = Date, y = Weight)) + geom_line()


# function to calculate the rolling average of weights
roll_avg <- function(wts){
  rollmean(wts, k =7, fill = NA)
}
roll_avg_wt <- roll_avg(all_cows_wt$Weight)
all_cows_avg_wt <- cbind(all_cows_wt, roll_avg_wt )

# plotting WOW date vs weights taking rolling average of 7 days
ggplot(data = all_cows_avg_wt, mapping = aes(x = Date, y = roll_avg_wt, color = RFID)) + geom_line()



############# Frequency plots ############
library(tidyverse)

# platting in facet
ggplot(all_cows_wt, mapping = aes(Date)) +
  geom_histogram() +
  facet_wrap(~ RFID)

# cow 1 weights for year 2022
cow1_wts <- all_cows_wt[all_cows_wt$RFID == "982 123724743516" & all_cows_wt$Date < "2022-05-30 00:00:00", ]

# ploting growth curve
ggplot(cow1_wts, mapping = aes(x = Date, y = Weight))+
  geom_point()

# ggplot(cow1_wts, mapping = aes(Date)) + geom_histogram() +
#   labs(x = "Date", y = "No. of weights", title = "Frequency of WOW weights for cow 1")









