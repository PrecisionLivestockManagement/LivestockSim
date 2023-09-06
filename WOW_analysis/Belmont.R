library(DMMongoDB)
library(tidyverse)
library(zoo)
library(scales)
library(lme4)
library(lubridate)
library(DescTools)
library(gridExtra)

### importing walk-over weights and static weights data 
dailywts_alltime <- read.csv(file = "Data/Belmont_dailywts_alltime.csv")
staticwts_alltime <- read.csv(file = "Data/Belmont_staticwts_alltime.csv")


# changing some column names and removing zero weights
dailywts_alltime$DateTime <- dailywts_alltime$Date
dailywts_alltime$Date <- as.Date(dailywts_alltime$Date, format = "%d-%m-%y")
colnames(dailywts_alltime)[colnames(dailywts_alltime) == "Weight"] <- "dailywt"
dailywts_raw <- dailywts_alltime[dailywts_alltime$dailywt > 0, ]

staticwts_alltime$Date <- as.Date(staticwts_alltime$Date)
staticwts_alltime <- staticwts_alltime[staticwts_alltime$Date != "0014-06-20", ]
colnames(staticwts_alltime)[colnames(staticwts_alltime) == "Weight"] <- "staticwt"
staticwts <- staticwts_alltime[staticwts_alltime$staticwt > 0, ]

# dividing animals into seperate herds based on timeline of wow collection
# herd1 (2017-05-01 to 2020-01-30; days of wow: , ) ####
dailywts_herd1 <- dailywts_raw[dailywts_raw$Date >= "2017-05-15" & dailywts_raw$Date <= "2020-01-30", ]

staticwts_herd1 <- staticwts %>%
  filter(RFID %in% unique(dailywts_herd1$RFID))

# selecting dailywts for animals of static weight (reference weight) available (891 animals)
dailywts_herd1_raw <- dailywts_herd1 %>%
  filter(RFID %in% unique(staticwts_herd1$RFID))

# selecting animals with at least 2 wow records
dailywts_herd1_raw <- dailywts_herd1_raw %>%
  group_by(RFID) %>%
  filter(n_distinct(Date) >= 2)

# averaging the static weights for each RFID to get the average reference weight
staticavg_herd1 <- staticwts_herd1 %>%
  group_by(RFID) %>%
  summarise(avg_staticwt = round(mean(staticwt), 2))

# dailywts_raw analysis
dailyavg_herd1_raw <- dailywts_herd1_raw %>%
  group_by(RFID, Date) %>%
  summarise(avg_dailywt = round(mean(dailywt), 2))

allwts_herd1_raw <- merge(dailyavg_herd1_raw, staticwts_herd1, by = c("RFID", "Date"))

ccc_herd1_raw <- CCC(allwts_herd1_raw$avg_dailywt, allwts_herd1_raw$staticwt)$rho.c

# outlier removal from WOW data
dailywts_herd1_out <- data.frame(RFID = character(), Date = character(), dailywt = numeric(), stringsAsFactors = FALSE)

for (i in unique(dailywts_herd1_raw$RFID)) {
  animal_data <- subset(dailywts_herd1_raw, RFID == i)
  
  ref_data <- staticavg_herd1 %>%
    filter(RFID == i)
    
  lower_limit <- ref_data$avg_staticwt /2
  upper_limit <- ref_data$avg_staticwt * 1.5
  
  filtered_animal_data <- subset(animal_data, dailywt >= lower_limit & dailywt <= upper_limit)
  dailywts_herd1_out <- rbind(dailywts_herd1_out, filtered_animal_data)
}

# averaging of WOWs if more than one weight recorded in a single day
dailyavg_herd1_out <- dailywts_herd1_out %>%
  group_by(RFID, Date) %>%
  summarise(avg_dailywt = round(mean(dailywt), 2))

allwts_herd1_out <- merge(dailyavg_herd1_out, staticwts_herd1, by = c("RFID", "Date"))

ccc_herd1_out <- CCC(allwts_herd1_out$avg_dailywt, allwts_herd1_out$staticwt)$rho.c

# linear interpolation to get the wows for missing days
dates_herd1 <- seq(as.Date("2017-05-15"), as.Date("2020-01-30"), by = "day")
dailywts_herd1_new <- expand.grid(RFID = unique(dailyavg_herd1_out$RFID), Date = dates_herd1)

dailywts_herd1_merged <- merge(dailywts_herd1_new, dailyavg_herd1_out, by = c("RFID", "Date"), all.x = TRUE)

dailywts_herd1_splited <- split(dailywts_herd1_merged, f = dailywts_herd1_merged$RFID)

dailywts_herd1_list <- list()

for (i in seq_along(dailywts_herd1_splited)) {
  dailywts_herd1_list[[i]] <- with(dailywts_herd1_splited[[i]], na.approx(x = Date, y = avg_dailywt, xout = dates_herd1)$y)
}

dailywts_herd1_est <- cbind(dailywts_herd1_merged, est_dailywt = unlist(dailywts_herd1_list))
dailywts_herd1_est$est_dailywt <- round(dailywts_herd1_est$est_dailywt, 2)

# smoothing dailywts using 13 days rolling average
dailywts_herd1_smooth <- data.frame(RFID = character(), Date = character(), smooth_dailywt = numeric(), stringsAsFactors = FALSE)

for (i in unique(dailywts_herd1_est$RFID)) {
  animal_data <- subset(dailywts_herd1_est, RFID == i)
  roll_avg <- rollmean(animal_data$est_dailywt, k = 13, align = 'center', na.pad = TRUE)
  merged_data <- cbind(animal_data, roll_avg)
  
  dailywts_herd1_smooth <- rbind(dailywts_herd1_smooth, merged_data)
}

dailywts_herd1_smooth$roll_avg <- round(dailywts_herd1_smooth$roll_avg, 2)
  

ggplot(daily)



