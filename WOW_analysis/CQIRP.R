library(DMMongoDB)
library(dplyr)
library(tidyverse)


# Getting static weight and daily weight data
CQIRP_static_wts <- get_staticwts(property = "CQIRP")
CQIRP_staticwts <- CQIRP_static_wts[CQIRP_static_wts$Weight > 0, ]

CQIRP_daily_wts <- get_dailywts(property = "CQIRP")
CQIRP_dailywts <- CQIRP_daily_wts[CQIRP_daily_wts$Weight > 0, ]
CQIRP_dailywts$stationname = "CQIRP"

# static wts and daily wts count over time
ggplot(CQIRP_staticwts, aes(Date)) +
  geom_histogram() +
  labs(title = "No. of static weights recorded over time")

ggplot(CQIRP_staticwts, aes(x = Date, y = Weight, color = RFID)) +
  geom_point() +
  theme(legend.position = "none") +
  labs(title = "Static weight records of each animal")

ggplot(CQIRP_dailywts, aes(Date)) +
  geom_histogram() +
  labs(title = "No. of daily weights recorded over time")

ggplot(CQIRP_dailywts, aes(x = Date, y = Weight, color = RFID)) +
  geom_point() +
  theme(legend.position = "none") +
  labs(title = "daily weight records of each animal")





###########################################################################################
### CQIRP animal data #
CQIRP_animals_current <- get_cattle(property = "CQIRP")
CQIRP_animals_all <- get_cattle(RFID = unique(CQIRP_daily_wts$RFID))

CQIRP_dailywts_current <- get_dailywts(RFID = CQIRP_animals_current$RFID)

# CQIRP_dailywts_all <- get_dailywts(property = "CQIRP")
# write.csv(CQIRP_dailywts_all, file = "WOW analysis/CQIRP_dailywts_all.csv")
# 
# CQIRP_stwts_all <- get_staticwts(property = "CQIRP")
# write.csv(CQIRP_stwts_all, file = "WOW analysis/CQIRP_stwts_all.csv")




#static weights #
stwts_counts <- c()

for (i in CQIRP_staticwts$RFID) {
  stwts_counts[i] <- sum(CQIRP_staticwts$RFID == i)
}

df_stwts <- data.frame(RFID = c(unique(CQIRP_staticwts$RFID)),
                 num_stwts = stwts_counts)

# summary statistics
summary(CQIRP_staticwts$Weight)
sd(CQIRP_staticwts$Weight)

# plots
ggplot(df_stwts, aes(x = num_stwts, color = RFID)) +
  geom_bar()

ggplot(CQIRP_staticwts, aes(x = Date, y = Weight, color = RFID)) +
  geom_line()

ggplot(CQIRP_staticwts, aes(x = RFID)) +
  geom_bar()


#### Walk-over weights #
length(unique(CQIRP_dailywts$RFID))

wow_counts <- c()
for(i in CQIRP_dailywts$RFID) {
  wow_counts[i] <- sum(CQIRP_dailywts$RFID == i)
}

df_wow <- data.frame(RFID = c(unique(CQIRP_dailywts$RFID)),
                       num_wow = wow_counts)

sum(df_wow$num_wow >= 1 & df_wow$num_wow <= 10)
sum(df_wow$num_wow >= 11 & df_wow$num_wow <= 100)
sum(df_wow$num_wow >= 101 & df_wow$num_wow <= 1000)
sum(df_wow$num_wow >= 1001)

summary(CQIRP_dailywts$Weight)
sd(CQIRP_dailywts$Weight)

ggplot(df_wow, aes(x = num_wow)) +
  geom_histogram()


# Selecting animals with both static weights and wow records 
RFIDsearch(RFID = CQIRP_staticwts$RFID)

wow_selected <- get_dailywts(RFID = subset_stwts$RFID)
wow_selected_rm0 <- subset(wow_selected[wow_selected$Weight != 0, ])

ggplot(wow_selected_rm0, aes(x = Date, y = Weight, color = RFID)) +
  geom_line()




















