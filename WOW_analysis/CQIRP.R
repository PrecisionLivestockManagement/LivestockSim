library(DMMongoDB)
library(tidyverse)


# Getting static weight and daily weight data #####
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

CQIRP_dailywts$DateTime <- CQIRP_dailywts$Date
CQIRP_dailywts$Date <- as.Date(CQIRP_dailywts$Date)
CQIRP_weights_all <- merge(CQIRP_dailywts, CQIRP_staticwts, by = c("RFID", "Date"), all.x = TRUE)

ggplot(CQIRP_weights_all, aes(x = Weight.x, y = Weight.y)) +
  geom_point() 


### CQIRP animal data #####
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











### CQIRP_animals_current (Accuracy and repeatability analysis) ####
CQIRP_animals_current <- get_cattle(property = "CQIRP")

# getting static wts
staticwts_ <- get_staticwts(RFID = CQIRP_animals_current$RFID)
summary(staticwts_$Weight)
sd(staticwts_$Weight)

dailywts_ <- get_dailywts(RFID = CQIRP_animals_current, start = "2022-04-29")
dailywts_ <- dailywts_[dailywts_$Date <= "2022-05-20", ]
summary(dailywts_$Weight)
sd(dailywts_$Weight)

# removing front foot weights, and weights below 40kg and above 600 kg
dailywts_ <- dailywts_[dailywts_$Location != "29.FrontFoot" & dailywts_$Weight > 40 & dailywts_$Weight < 600 , ]
summary(dailywts_$Weight)
sd(dailywts_$Weight)

# averaging the daily wts for each day
dailywts_$DateTime <- dailywts_$Date
dailywts_$Date <- as.Date(dailywts_$Date)

avg_dailywts_ <- dailywts_ %>%
  group_by(RFID, Date) %>%
  summarise(avg_dailywts = mean(Weight))
summary(avg_dailywts_$avg_dailywts)
sd(avg_dailywts_$avg_dailywts)


ggplot(staticwts_, aes(x = Date, y = Weight, color = RFID))+
  geom_line()



## getting static weight for each day using linear-interpoloation
library(lubridate)

all_dates <- seq(min(staticwts_$Date), max(staticwts_$Date), by = "day")
new_staticwts_ <- expand.grid(RFID = unique(staticwts_$RFID), Date = all_dates)

merged_staticwts_ <- merge(new_staticwts_, staticwts_, by = c("RFID", "Date"), all.x = TRUE)

staticwts_splited <- split(merged_staticwts_, f = merged_staticwts_$RFID)

# static weights for each animals for all days in the paddock

est_staticwts_1 <- with(staticwts_splited[[1]], approx(x = Date, y = Weight, xout = all_dates)$y)
est_staticwts_2 <- with(staticwts_splited[[2]], approx(x = Date, y = Weight, xout = all_dates)$y)
est_staticwts_3 <- with(staticwts_splited[[3]], approx(x = Date, y = Weight, xout = all_dates)$y)
est_staticwts_4 <- with(staticwts_splited[[4]], approx(x = Date, y = Weight, xout = all_dates)$y)
est_staticwts_5 <- with(staticwts_splited[[5]], approx(x = Date, y = Weight, xout = all_dates)$y)
est_staticwts_6 <- with(staticwts_splited[[6]], approx(x = Date, y = Weight, xout = all_dates)$y)
est_staticwts_7 <- with(staticwts_splited[[7]], approx(x = Date, y = Weight, xout = all_dates)$y)
est_staticwts_8 <- with(staticwts_splited[[8]], approx(x = Date, y = Weight, xout = all_dates)$y)

est_staticwts_ <- cbind(merged_staticwts_, est_staticwts = c(est_staticwts_1, est_staticwts_2, est_staticwts_3, est_staticwts_4, est_staticwts_5, est_staticwts_6, est_staticwts_7, est_staticwts_8))

ggplot(est_staticwts_, aes(x = Date, y = est_staticwts, color = RFID)) +
  geom_point()


# merging daily wts and static weights data and plotting
all_weights_ <- merge(avg_dailywts_, est_staticwts_, by = c("RFID", "Date"), all.x = FALSE)


ggplot(all_weights_, aes(x = Date, color = RFID)) +
  geom_line(aes(y = est_staticwts)) +
  geom_point(aes(y = avg_dailywts))

ggplot(all_weights_, aes(x = Date)) +
  geom_line(aes(y = est_staticwts)) +
  geom_point(aes(y = avg_dailywts)) +
  facet_wrap(~RFID) +
  labs(title = "Plot of static weights and daily wts for each cow", y = "Weight (kg)")

# differences between static weights and wow
all_weights_$diff <- c(all_weights_$avg_dailywts - all_weights_$est_staticwts)
all_weights_$diff_percent <- abs(all_weights_$diff)/all_weights_$est_staticwts * 100

ggplot(all_weights_, aes(diff)) +
  geom_histogram(color = "black") +
  scale_x_continuous(breaks = seq(-100, 150, 20)) +
  labs(title = "Difference of daily weight form static weights", x = "average daily weight minus static weight(kg)", y = "Number of weights")

ggplot(all_weights_, aes(x = est_staticwts, y = avg_dailywts)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Static wts vs daily wts for CQIRP animals")


# calculating concordance correlation coefficient to assess degree of agreement between static weights and daily wts
library(DescTools)

ccc_result <-CCC(all_weights_$avg_dailywts, all_weights_$est_staticwts)
ccc_value <- ccc_result[["rho.c"]]




cor(all_weights_$avg_dailywts, all_weights_$est_staticwts, method = "spearman")




############### Tremere ###############
Tremere_animals_current <- get_cattle(property = "Tremere")
Tremere_staticwts_ <- get_staticwts(RFID = Tremere_animals_current$RFID)
Tremere_dailywts_ <- get_dailywts(RFID = Tremere_animals_current$RFID)

ggplot(Tremere_staticwts_, aes(x = Date, y = Weight, color = RFID)) +
  geom_line() +
  guides(color = "none")

ggplot(Tremere_dailywts_, aes(x = Date, y = Weight, color = RFID)) +
  geom_line() +
  guides(color = "none")


Tremere_dailywts_$DateTime <- Tremere_dailywts_$Date
Tremere_dailywts_$Date <- as.Date(Tremere_dailywts_$Date)

Tremere_avg_dailywts_ <- Tremere_dailywts_ %>%
  group_by(RFID, Date) %>%
  summarise(avg_dailywts = mean(Weight))

Tremere_allwts_ <- merge(Tremere_avg_dailywts_, Tremere_staticwts_, by = c("RFID", "Date"))
























