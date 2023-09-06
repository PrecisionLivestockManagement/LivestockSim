library(DMMongoDB)
library(tidyverse)
library(zoo)
library(scales)
library(lme4)
library(lubridate)
library(DescTools)
library(gridExtra)


### importing walk-over weights and static weights data 
dailywts_all <- read.csv(file = "Data/Belmont_dailywts.csv")
staticwts_all <- read.csv(file = "Data/Belmont_staticwts.csv")


# changing some column names and removing zero weights
dailywts_all <- select(dailywts_all, -X)
colnames(dailywts_all)[colnames(dailywts_all) == "Date"] <- "DateTime"
dailywts_all$Date <- as.Date(dailywts_all$DateTime)
colnames(dailywts_all)[colnames(dailywts_all) == "Weight"] <- "dailywt"
dailywts_raw <- dailywts_all[dailywts_all$dailywt > 0, ]

staticwts_all <- select(staticwts_all, -X)
colnames(staticwts_all)[colnames(staticwts_all) == "Weight"] <- "staticwt"
staticwts <- staticwts_all[staticwts_all$staticwt > 0, ]

# dailywts_raw analysis
avg_dailywts_raw <- dailywts_raw %>%
  group_by(RFID, Date) %>%
  summarise(avg_dailywt = round(mean(dailywt), 2))

allwts_raw <- merge(avg_dailywts_raw, staticwts, by = c("RFID", "Date"))

ccc_raw <- CCC(allwts_raw$avg_dailywt, allwts_raw$staticwt)$rho.c

# outlier removal from WOW data
dailywts_out <- data.frame(RFID = character(), Date = character(), dailywt = numeric(), stringsAsFactors = FALSE)

for (i in unique(dailywts_raw$RFID)) {
  animal_data <- subset(dailywts_raw, RFID == i)
  
  Q1 <- quantile(animal_data$dailywt, 0.25, na.rm = TRUE)
  Q3 <- quantile(animal_data$dailywt, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower_limit <- Q1 - 1.5 * IQR
  upper_limit <- Q3 + 1.5 * IQR
  
  filtered_animal_data <- subset(animal_data, dailywt >= lower_limit & dailywt <= upper_limit)
  dailywts_out <- rbind(dailywts_out, filtered_animal_data)
}


# averaging of WOWs if more than one weight recorded in a single day
avg_dailywts_out <- dailywts_out %>%
  group_by(RFID, Date) %>%
  summarise(avg_dailywt = round(mean(dailywt), 2))

allwts_out <- merge(avg_dailywts_out, staticwts, by = c("RFID", "Date"))

ccc_out <- CCC(allwts_out$avg_dailywt, allwts_out$staticwt)$rho.c


# dividing animals into seperate herds
# herd1 (2020-11-17 to 2021-01-28; Location: Kraatz; days of wow: 73, no of animals = 41) ####
dailywts_herd1_raw <- dailywts_raw[dailywts_raw$Date >= "2020-11-17" & dailywts_raw$Date <= "2021-01-28", ]
dailywts_herd1_raw <- dailywts_herd1_raw %>%
  group_by(RFID) %>%
  filter(n_distinct(Date) >= 13)

# outlier removal
dailywts_herd1_out <- data.frame(RFID = character(), Date = character(), dailywt = numeric(), stringsAsFactors = FALSE)

for (i in unique(dailywts_herd1_raw$RFID)) {
  animal_data <- subset(dailywts_herd1_raw, RFID == i)
  
  Q1 <- quantile(animal_data$dailywt, 0.25, na.rm = TRUE)
  Q3 <- quantile(animal_data$dailywt, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower_limit <- Q1 - 1.5 * IQR
  upper_limit <- Q3 + 1.5 * IQR
  
  filtered_animal_data <- subset(animal_data, dailywt >= lower_limit & dailywt <= upper_limit)
  dailywts_herd1_out <- rbind(dailywts_herd1_out, filtered_animal_data)
}

# averaging of WOWs if more than one weight recorded in a single day
dailywts_herd1_avg <- dailywts_herd1_out %>%
  group_by(RFID, Date) %>%
  summarise(avg_dailywt = round(mean(dailywt), 2))


# linear interpolation to get the wows for missing days
dates_herd1 <- seq(as.Date("2020-11-17"), as.Date("2021-01-28"), by = "day")
dailywts_herd1_new <- expand.grid(RFID = unique(dailywts_herd1_avg$RFID), Date = dates_herd1)

dailywts_herd1_merged <- merge(dailywts_herd1_new, dailywts_herd1_avg, by = c("RFID", "Date"), all.x = TRUE)

dailywts_herd1_splited <- split(dailywts_herd1_merged, f = dailywts_herd1_merged$RFID)

dailywts_herd1_list <- list()

for (i in seq_along(dailywts_herd1_splited)) {
  dailywts_herd1_list[[i]] <- with(dailywts_herd1_splited[[i]], approx(x = Date, y = avg_dailywt, xout = dates_herd1)$y)
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


# herd2 (2021-09-20 to 2022-02-23; Location: various; days of wow: 157, no of animals = 71) ####
dailywts_herd2_raw <- dailywts_raw[dailywts_raw$Date >= "2021-09-20" & dailywts_raw$Date <= "2022-02-23", ]
dailywts_herd2_raw <- dailywts_herd2_raw %>%
  group_by(RFID) %>%
  filter(n_distinct(Date) >= 13)

# outlier removal
dailywts_herd2_out <- data.frame(RFID = character(), Date = character(), dailywt = numeric(), stringsAsFactors = FALSE)

for (i in unique(dailywts_herd2_raw$RFID)) {
  animal_data <- subset(dailywts_herd2_raw, RFID == i)
  
  Q1 <- quantile(animal_data$dailywt, 0.25, na.rm = TRUE)
  Q3 <- quantile(animal_data$dailywt, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower_limit <- Q1 - 1.5 * IQR
  upper_limit <- Q3 + 1.5 * IQR
  
  filtered_animal_data <- subset(animal_data, dailywt >= lower_limit & dailywt <= upper_limit)
  dailywts_herd2_out <- rbind(dailywts_herd2_out, filtered_animal_data)
}

# averaging of WOWs if more than one weight recorded in a single day
dailywts_herd2_avg <- dailywts_herd2_out %>%
  group_by(RFID, Date) %>%
  summarise(avg_dailywt = round(mean(dailywt), 2))


# linear interpolation to get the wows for missing days
dates_herd2 <- seq(as.Date("2021-09-20"), as.Date("2022-02-23"), by = "day")
dailywts_herd2_new <- expand.grid(RFID = unique(dailywts_herd2_avg$RFID), Date = dates_herd2)

dailywts_herd2_merged <- merge(dailywts_herd2_new, dailywts_herd2_avg, by = c("RFID", "Date"), all.x = TRUE)

dailywts_herd2_splited <- split(dailywts_herd2_merged, f = dailywts_herd2_merged$RFID)

dailywts_herd2_list <- list()

for (i in seq_along(dailywts_herd2_splited)) {
  dailywts_herd2_list[[i]] <- with(dailywts_herd2_splited[[i]], approx(x = Date, y = avg_dailywt, xout = dates_herd2)$y)
}

dailywts_herd2_est <- cbind(dailywts_herd2_merged, est_dailywt = unlist(dailywts_herd2_list))
dailywts_herd2_est$est_dailywt <- round(dailywts_herd2_est$est_dailywt, 2)

# smoothing dailywts using 13 days rolling average
dailywts_herd2_smooth <- data.frame(RFID = character(), Date = character(), smooth_dailywt = numeric(), stringsAsFactors = FALSE)

for (i in unique(dailywts_herd2_est$RFID)) {
  animal_data <- subset(dailywts_herd2_est, RFID == i)
  roll_avg <- rollmean(animal_data$est_dailywt, k = 13, align = 'center', na.pad = TRUE)
  merged_data <- cbind(animal_data, roll_avg)
  
  dailywts_herd2_smooth <- rbind(dailywts_herd2_smooth, merged_data)
}

dailywts_herd2_smooth$roll_avg <- round(dailywts_herd2_smooth$roll_avg, 2)


# herd3 (2022-08-03 to 2023-04-29; Location: DM15Final and DM13BelKraatz; days of wow: 270, no of animals = 186) ####
dailywts_herd3_raw <- dailywts_raw[dailywts_raw$Date >= "2022-08-03" & dailywts_raw$Date <= "2023-04-29", ]
dailywts_herd3_raw <- dailywts_herd3_raw[dailywts_herd3_raw$Location == "DM13BelKraatz" | dailywts_herd3_raw$Location == "DM15Final", ]

# removing 	982 123736288668, as it is added to this herd only after feb 2023
dailywts_herd3_raw <- dailywts_herd3_raw[dailywts_herd3_raw$RFID != "982 123736288668", ]

dailywts_herd3_raw <- dailywts_herd3_raw %>%
  group_by(RFID) %>%
  filter(n_distinct(Date) >= 13)

# outlier removal
dailywts_herd3_out <- data.frame(RFID = character(), Date = character(), dailywt = numeric(), stringsAsFactors = FALSE)

for (i in unique(dailywts_herd3_raw$RFID)) {
  animal_data <- subset(dailywts_herd3_raw, RFID == i)
  
  Q1 <- quantile(animal_data$dailywt, 0.25, na.rm = TRUE)
  Q3 <- quantile(animal_data$dailywt, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower_limit <- Q1 - 1.5 * IQR
  upper_limit <- Q3 + 1.5 * IQR
  
  filtered_animal_data <- subset(animal_data, dailywt >= lower_limit & dailywt <= upper_limit)
  dailywts_herd3_out <- rbind(dailywts_herd3_out, filtered_animal_data)
}

# averaging of WOWs if more than one weight recorded in a single day
dailywts_herd3_avg <- dailywts_herd3_out %>%
  group_by(RFID, Date) %>%
  summarise(avg_dailywt = round(mean(dailywt), 2))

dailywts_herd3_avg <- dailywts_herd3_avg %>%
  group_by(RFID) %>%
  filter(n_distinct(Date) >= 2)


# linear interpolation to get the wows for missing days
dates_herd3 <- seq(as.Date("2022-08-03"), as.Date("2023-04-29"), by = "day")
dailywts_herd3_new <- expand.grid(RFID = unique(dailywts_herd3_avg$RFID), Date = dates_herd3)

dailywts_herd3_merged <- merge(dailywts_herd3_new, dailywts_herd3_avg, by = c("RFID", "Date"), all.x = TRUE)

dailywts_herd3_splited <- split(dailywts_herd3_merged, f = dailywts_herd3_merged$RFID)

dailywts_herd3_list <- list()

for (i in seq_along(dailywts_herd3_splited)) {
  dailywts_herd3_list[[i]] <- with(dailywts_herd3_splited[[i]], approx(x = Date, y = avg_dailywt, xout = dates_herd3)$y)
}

dailywts_herd3_est <- cbind(dailywts_herd3_merged, est_dailywt = unlist(dailywts_herd3_list))
dailywts_herd3_est$est_dailywt <- round(dailywts_herd3_est$est_dailywt, 2)

# smoothing dailywts using 13 days rolling average
dailywts_herd3_smooth <- data.frame(RFID = character(), Date = character(), smooth_dailywt = numeric(), stringsAsFactors = FALSE)

for (i in unique(dailywts_herd3_est$RFID)) {
  animal_data <- subset(dailywts_herd3_est, RFID == i)
  roll_avg <- rollmean(animal_data$est_dailywt, k = 13, align = 'center', na.pad = TRUE)
  merged_data <- cbind(animal_data, roll_avg)
  
  dailywts_herd3_smooth <- rbind(dailywts_herd3_smooth, merged_data)
}

dailywts_herd3_smooth$roll_avg <- round(dailywts_herd3_smooth$roll_avg, 2)


# Summary statistics ####
# No. of animals
length(unique(dailywts_herd1_raw$RFID))
length(unique(dailywts_herd2_raw$RFID))
length(unique(dailywts_herd3_raw$RFID))
length(unique(dailywts_herd1_raw$RFID)) + length(unique(dailywts_herd2_raw$RFID)) + length(unique(dailywts_herd3_raw$RFID))

# Days of data capture
as.Date("2021-01-28") - as.Date("2020-11-17") + 1
as.Date("2022-02-23") - as.Date("2021-09-20") + 1
as.Date("2023-04-29") - as.Date("2022-08-03") + 1
as.Date("2021-01-28") - as.Date("2020-11-17") + as.Date("2022-02-23") - as.Date("2021-09-20") + as.Date("2023-04-29") - as.Date("2022-08-03") + 3

# No. of weight records
length(na.omit(dailywts_herd1_raw$dailywt))
length(na.omit(dailywts_herd2_raw$dailywt))
length(na.omit(dailywts_herd3_raw$dailywt))
length(na.omit(dailywts_herd1_raw$dailywt)) + length(na.omit(dailywts_herd2_raw$dailywt)) + length(na.omit(dailywts_herd3_raw$dailywt))

length(na.omit(dailywts_herd1_out$dailywt))
length(na.omit(dailywts_herd2_out$dailywt))
length(na.omit(dailywts_herd3_out$dailywt))
length(na.omit(dailywts_herd1_out$dailywt)) + length(na.omit(dailywts_herd2_out$dailywt)) + length(na.omit(dailywts_herd3_out$dailywt))

length(na.omit(dailywts_herd1_smooth$roll_avg))
length(na.omit(dailywts_herd2_smooth$roll_avg))
length(na.omit(dailywts_herd3_smooth$roll_avg))
length(na.omit(dailywts_herd1_smooth$roll_avg)) + length(na.omit(dailywts_herd2_smooth$roll_avg)) + length(na.omit(dailywts_herd3_smooth$roll_avg))

# Average of mean, minimum, maximum, SD and CV of LW
# herd 1
summary_herd1_raw <- dailywts_herd1_raw %>%
  group_by(RFID) %>%
  summarise(mean = round(mean(dailywt), 2),
            min = round(min(dailywt), 2),
            max = round(max(dailywt), 2),
            sd = round(sd(dailywt), 2),
            cv = round(sd(dailywt)/mean(dailywt)*100, 2))

for (i in names(summary_herd1_raw[, 2:6])) {
  avg <- round(mean(summary_herd1_raw[[i]]), 2)
  sd <- round(sd(summary_herd1_raw[[i]]), 2)
  print(data.frame(stat = i, avg, sd))
}

summary_herd1_out <- dailywts_herd1_out %>%
  group_by(RFID) %>%
  summarise(mean = round(mean(dailywt), 2),
            min = round(min(dailywt), 2),
            max = round(max(dailywt), 2),
            sd = round(sd(dailywt), 2),
            cv = round(sd(dailywt)/mean(dailywt)*100, 2))

for (i in names(summary_herd1_out[, 2:6])) {
  avg <- round(mean(summary_herd1_out[[i]]), 2)
  sd <- round(sd(summary_herd1_out[[i]]), 2)
  print(data.frame(stat = i, avg, sd))
}


summary_herd1_smooth <- dailywts_herd1_smooth %>%
  group_by(RFID) %>%
  summarise(mean = round(mean(roll_avg, na.rm = TRUE), 2),
            min = round(min(roll_avg, na.rm = TRUE), 2),
            max = round(max(roll_avg, na.rm = TRUE), 2),
            sd = round(sd(roll_avg, na.rm = TRUE), 2),
            cv = round(sd(roll_avg, na.rm = TRUE)/mean(roll_avg, na.rm = TRUE)*100, 2))

for (i in names(summary_herd1_smooth[, 2:6])) {
  avg <- round(mean(summary_herd1_smooth[[i]]), 2)
  sd <- round(sd(summary_herd1_smooth[[i]]), 2)
  print(data.frame(stat = i, avg, sd))
}


# herd 2
summary_herd2_raw <- dailywts_herd2_raw %>%
  group_by(RFID) %>%
  summarise(mean = round(mean(dailywt), 2),
            min = round(min(dailywt), 2),
            max = round(max(dailywt), 2),
            sd = round(sd(dailywt), 2),
            cv = round(sd(dailywt)/mean(dailywt)*100, 2))

for (i in names(summary_herd2_raw[, 2:6])) {
  avg <- round(mean(summary_herd2_raw[[i]]), 2)
  sd <- round(sd(summary_herd2_raw[[i]]), 2)
  print(data.frame(stat = i, avg, sd))
}

summary_herd2_out <- dailywts_herd2_out %>%
  group_by(RFID) %>%
  summarise(mean = round(mean(dailywt), 2),
            min = round(min(dailywt), 2),
            max = round(max(dailywt), 2),
            sd = round(sd(dailywt), 2),
            cv = round(sd(dailywt)/mean(dailywt)*100, 2))

for (i in names(summary_herd2_out[, 2:6])) {
  avg <- round(mean(summary_herd2_out[[i]]), 2)
  sd <- round(sd(summary_herd2_out[[i]]), 2)
  print(data.frame(stat = i, avg, sd))
}


summary_herd2_smooth <- dailywts_herd2_smooth %>%
  group_by(RFID) %>%
  summarise(mean = round(mean(roll_avg, na.rm = TRUE), 2),
            min = round(min(roll_avg, na.rm = TRUE), 2),
            max = round(max(roll_avg, na.rm = TRUE), 2),
            sd = round(sd(roll_avg, na.rm = TRUE), 2),
            cv = round(sd(roll_avg, na.rm = TRUE)/mean(roll_avg, na.rm = TRUE)*100, 2))

for (i in names(summary_herd2_smooth[, 2:6])) {
  avg <- round(mean(summary_herd2_smooth[[i]]), 2)
  sd <- round(sd(summary_herd2_smooth[[i]]), 2)
  print(data.frame(stat = i, avg, sd))
}

# herd 3
summary_herd3_raw <- dailywts_herd3_raw %>%
  group_by(RFID) %>%
  summarise(mean = round(mean(dailywt), 2),
            min = round(min(dailywt), 2),
            max = round(max(dailywt), 2),
            sd = round(sd(dailywt), 2),
            cv = round(sd(dailywt)/mean(dailywt)*100, 2))

for (i in names(summary_herd3_raw[, 2:6])) {
  avg <- round(mean(summary_herd3_raw[[i]]), 2)
  sd <- round(sd(summary_herd3_raw[[i]]), 2)
  print(data.frame(stat = i, avg, sd))
}

summary_herd3_out <- dailywts_herd3_out %>%
  group_by(RFID) %>%
  summarise(mean = round(mean(dailywt), 2),
            min = round(min(dailywt), 2),
            max = round(max(dailywt), 2),
            sd = round(sd(dailywt), 2),
            cv = round(sd(dailywt)/mean(dailywt)*100, 2))

for (i in names(summary_herd3_out[, 2:6])) {
  avg <- round(mean(summary_herd3_out[[i]]), 2)
  sd <- round(sd(summary_herd3_out[[i]]), 2)
  print(data.frame(stat = i, avg, sd))
}


summary_herd3_smooth <- dailywts_herd3_smooth %>%
  group_by(RFID) %>%
  summarise(mean = round(mean(roll_avg, na.rm = TRUE), 2),
            min = round(min(roll_avg, na.rm = TRUE), 2),
            max = round(max(roll_avg, na.rm = TRUE), 2),
            sd = round(sd(roll_avg, na.rm = TRUE), 2),
            cv = round(sd(roll_avg, na.rm = TRUE)/mean(roll_avg, na.rm = TRUE)*100, 2))

for (i in names(summary_herd3_smooth[, 2:6])) {
  avg <- round(mean(summary_herd3_smooth[[i]]), 2)
  sd <- round(sd(summary_herd3_smooth[[i]]), 2)
  print(data.frame(stat = i, avg, sd))
}

# LW plots ####
# example liveweight curve for animal 
example_dailywts_raw <- dailywts_herd3_raw[dailywts_herd3_raw$RFID == "982 123766109049", ]
example_dailyavg_raw <- example_dailywts_raw %>%
  group_by(RFID, Date) %>%
  summarise(dailyavg_raw = round(mean(dailywt), 2))


example_dailywts_out <- dailywts_herd3_out[dailywts_herd3_out$RFID == "982 123766109049", ]
example_dailyavg_out <- example_dailywts_out %>%
  group_by(RFID, Date) %>%
  summarise(dailyavg_out = round(mean(dailywt), 2))

example_dailywts_smooth <- dailywts_herd3_smooth[dailywts_herd3_smooth$RFID == "982 123766109049", ]
example_dailywts_smooth <- example_dailywts_smooth[, c(1, 2, 5)]

example_dailywts_all <- merge(merge(example_dailyavg_raw, example_dailyavg_out, by = c("RFID", "Date"), all.x = TRUE, all.y = TRUE), example_dailywts_smooth, by = c("RFID", "Date"), all.x = TRUE, all.y = TRUE)

ggplot() +
  geom_point(data = example_dailywts_raw, aes(x = Date, y = dailywt, color = "Outlier weights")) +
  geom_point(data = example_dailywts_out, aes(x = Date, y = dailywt, color = "Usable weights")) +
  geom_line(data = example_dailywts_smooth, aes(x = Date, y = roll_avg)) +
  guides(color = "none") +
  scale_x_date(date_labels = "%d", date_breaks = "10 day")+ 
  labs(y = "liveweight (kg)")

# liveweight plots for herd 1 herd 2 and herd 3
avg_dailywts_herd1 <- dailywts_herd1_smooth %>%
  group_by(Date) %>%
  summarize(avg_dailywt = round(mean(roll_avg, na.rm = TRUE), 2))

plot_herd1 <- ggplot() +
  geom_point(data = dailywts_herd1_smooth, aes(x = Date, y = roll_avg)) +
  scale_x_date(date_labels = "%m-%d", date_breaks = "10 days") +
  geom_line(data = avg_dailywts_herd1, aes(x = Date, y = avg_dailywt), size = 1, color = "blue") +
  labs(title = "Herd 1", x = "Date (Year 2020 - 2021)", y = "Smoothed liveweight (kg)") +
  theme(plot.title = element_text(hjust = 0.5))

plot_herd1

avg_dailywts_herd2 <- dailywts_herd2_smooth %>%
  group_by(Date) %>%
  summarize(avg_dailywt = round(mean(roll_avg, na.rm = TRUE), 2))

plot_herd2 <- ggplot() +
  geom_point(data = dailywts_herd2_smooth, aes(x = Date, y = roll_avg)) +
  geom_line(data = avg_dailywts_herd2, aes(x = Date, y = avg_dailywt), size = 1, color = "blue") +
  scale_x_date(date_labels = "%m-%d", date_breaks = "15 days") +
  labs(title = "Herd 2", x = "Date (Year 2021 - 2022)", y = "Smoothed liveweight (kg)") +
  theme(plot.title = element_text(hjust = 0.5))

plot_herd2

avg_dailywts_herd3 <- dailywts_herd3_smooth %>%
  group_by(Date) %>%
  summarize(avg_dailywt = round(mean(roll_avg, na.rm = TRUE), 2))

plot_herd3 <- ggplot() +
  geom_point(data = dailywts_herd3_smooth, aes(x = Date, y = roll_avg)) +
  scale_x_date(date_labels = "%m-%d", date_breaks = "30 days") +
  geom_line(data = avg_dailywts_herd3, aes(x = Date, y = avg_dailywt), size = 1, color = "blue") +
  labs(title = "Herd 3", x = "Date (Year 2022 - 2023)", y = "Smoothed liveweight (kg)") +
  theme(plot.title = element_text(hjust = 0.5))

plot_herd3


plot_herd1_herd2_herd3 <- grid.arrange(plot_herd1, plot_herd2, plot_herd3, nrow = 1)


# LWC analysis ####
# Herd 1 - raw data
dailyavg_herd1_raw <- dailywts_herd1_raw %>%
  group_by(RFID, Date) %>%
  summarise(dailyavg_raw = round(mean(dailywt), 2)) %>%
  ungroup()

dailyavg_herd1_raw_merged <- merge(dailywts_herd1_new, dailyavg_herd1_raw, by = c("RFID", "Date"), all.x = TRUE)

dailyavg_herd1_raw_splited <- split(dailyavg_herd1_raw_merged, f = dailyavg_herd1_raw_merged$RFID)

dailyavg_herd1_raw_list <- list()

for (i in seq_along(dailyavg_herd1_raw_splited)) {
  dailyavg_herd1_raw_list[[i]] <- with(dailyavg_herd1_raw_splited[[i]], approx(x = Date, y = dailyavg_raw, xout = dates_herd1)$y)
}

dailyavg_herd1_raw_est <- cbind(dailyavg_herd1_raw_merged, est_dailywt = unlist(dailyavg_herd1_raw_list))
dailyavg_herd1_raw_est$est_dailywt <- round(dailyavg_herd1_raw_est$est_dailywt, 2)

LWC_herd1_raw <- dailyavg_herd1_raw_est %>%
  group_by(RFID) %>%
  mutate(daily_lwc = c(NA, diff(est_dailywt))) %>%
  ungroup()


summary_LWC_herd1_raw <- LWC_herd1_raw %>%
  group_by(RFID) %>%
  summarise(mean = round(mean(daily_lwc, na.rm = TRUE), 2),
            min = round(min(daily_lwc, na.rm = TRUE), 2),
            max = round(max(daily_lwc, na.rm = TRUE), 2),
            sd = round(sd(daily_lwc, na.rm = TRUE), 2),
            cv = round(sd(daily_lwc, na.rm = TRUE)/mean(daily_lwc, na.rm = TRUE)*100, 2))

for (i in names(summary_LWC_herd1_raw[, 2:6])) {
  avg <- round(mean(summary_LWC_herd1_raw[[i]]), 2)
  sd <- round(sd(summary_LWC_herd1_raw[[i]]), 2)
  print(data.frame(stat = i, avg, sd))
}

# herd 1 -oulier removed data

LWC_herd1_out <- dailywts_herd1_est %>%
  group_by(RFID) %>%
  mutate(daily_lwc = c(NA, diff(est_dailywt))) %>%
  ungroup()


summary_LWC_herd1_out <- LWC_herd1_out %>%
  group_by(RFID) %>%
  summarise(mean = round(mean(daily_lwc, na.rm = TRUE), 2),
            min = round(min(daily_lwc, na.rm = TRUE), 2),
            max = round(max(daily_lwc, na.rm = TRUE), 2),
            sd = round(sd(daily_lwc, na.rm = TRUE), 2),
            cv = round(sd(daily_lwc, na.rm = TRUE)/mean(daily_lwc, na.rm = TRUE)*100, 2))

for (i in names(summary_LWC_herd1_out[, 2:6])) {
  avg <- round(mean(summary_LWC_herd1_out[[i]]), 2)
  sd <- round(sd(summary_LWC_herd1_out[[i]]), 2)
  print(data.frame(stat = i, avg, sd))
}

# herd 1 - smoothed data
LWC_herd1_smooth <- dailywts_herd1_smooth %>%
  group_by(RFID) %>%
  mutate(daily_lwc = c(NA, diff(roll_avg))) %>%
  ungroup()

summary_LWC_herd1_smooth <- LWC_herd1_smooth %>%
  group_by(RFID) %>%
  summarise(mean = round(mean(daily_lwc, na.rm = TRUE), 2),
            min = round(min(daily_lwc, na.rm = TRUE), 2),
            max = round(max(daily_lwc, na.rm = TRUE), 2),
            sd = round(sd(daily_lwc, na.rm = TRUE), 2),
            cv = round(sd(daily_lwc, na.rm = TRUE)/mean(daily_lwc, na.rm = TRUE)*100, 2))

for (i in names(summary_LWC_herd1_smooth[, 2:6])) {
  avg <- round(mean(summary_LWC_herd1_smooth[[i]]), 2)
  sd <- round(sd(summary_LWC_herd1_smooth[[i]]), 2)
  print(data.frame(stat = i, avg, sd))
}


# Herd 2 - raw data
dailyavg_herd2_raw <- dailywts_herd2_raw %>%
  group_by(RFID, Date) %>%
  summarise(dailyavg_raw = round(mean(dailywt), 2)) %>%
  ungroup()

dailyavg_herd2_raw_merged <- merge(dailywts_herd2_new, dailyavg_herd2_raw, by = c("RFID", "Date"), all.x = TRUE)

dailyavg_herd2_raw_splited <- split(dailyavg_herd2_raw_merged, f = dailyavg_herd2_raw_merged$RFID)

dailyavg_herd2_raw_list <- list()

for (i in seq_along(dailyavg_herd2_raw_splited)) {
  dailyavg_herd2_raw_list[[i]] <- with(dailyavg_herd2_raw_splited[[i]], approx(x = Date, y = dailyavg_raw, xout = dates_herd2)$y)
}

dailyavg_herd2_raw_est <- cbind(dailyavg_herd2_raw_merged, est_dailywt = unlist(dailyavg_herd2_raw_list))
dailyavg_herd2_raw_est$est_dailywt <- round(dailyavg_herd2_raw_est$est_dailywt, 2)

LWC_herd2_raw <- dailyavg_herd2_raw_est %>%
  group_by(RFID) %>%
  mutate(daily_lwc = c(NA, diff(est_dailywt))) %>%
  ungroup()


summary_LWC_herd2_raw <- LWC_herd2_raw %>%
  group_by(RFID) %>%
  summarise(mean = round(mean(daily_lwc, na.rm = TRUE), 2),
            min = round(min(daily_lwc, na.rm = TRUE), 2),
            max = round(max(daily_lwc, na.rm = TRUE), 2),
            sd = round(sd(daily_lwc, na.rm = TRUE), 2),
            cv = round(sd(daily_lwc, na.rm = TRUE)/mean(daily_lwc, na.rm = TRUE)*100, 2))

for (i in names(summary_LWC_herd2_raw[, 2:6])) {
  avg <- round(mean(summary_LWC_herd2_raw[[i]]), 2)
  sd <- round(sd(summary_LWC_herd2_raw[[i]]), 2)
  print(data.frame(stat = i, avg, sd))
}

# herd 2 -oulier removed data
LWC_herd2_out <- dailywts_herd2_est %>%
  group_by(RFID) %>%
  mutate(daily_lwc = c(NA, diff(est_dailywt))) %>%
  ungroup()


summary_LWC_herd2_out <- LWC_herd2_out %>%
  group_by(RFID) %>%
  summarise(mean = round(mean(daily_lwc, na.rm = TRUE), 2),
            min = round(min(daily_lwc, na.rm = TRUE), 2),
            max = round(max(daily_lwc, na.rm = TRUE), 2),
            sd = round(sd(daily_lwc, na.rm = TRUE), 2),
            cv = round(sd(daily_lwc, na.rm = TRUE)/mean(daily_lwc, na.rm = TRUE)*100, 2))

for (i in names(summary_LWC_herd2_out[, 2:6])) {
  avg <- round(mean(summary_LWC_herd2_out[[i]]), 2)
  sd <- round(sd(summary_LWC_herd2_out[[i]]), 2)
  print(data.frame(stat = i, avg, sd))
}

# herd 2 - smoothed data
LWC_herd2_smooth <- dailywts_herd2_smooth %>%
  group_by(RFID) %>%
  mutate(daily_lwc = c(NA, diff(roll_avg))) %>%
  ungroup()

summary_LWC_herd2_smooth <- LWC_herd2_smooth %>%
  group_by(RFID) %>%
  summarise(mean = round(mean(daily_lwc, na.rm = TRUE), 2),
            min = round(min(daily_lwc, na.rm = TRUE), 2),
            max = round(max(daily_lwc, na.rm = TRUE), 2),
            sd = round(sd(daily_lwc, na.rm = TRUE), 2),
            cv = round(sd(daily_lwc, na.rm = TRUE)/mean(daily_lwc, na.rm = TRUE)*100, 2))

for (i in names(summary_LWC_herd2_smooth[, 2:6])) {
  avg <- round(mean(summary_LWC_herd2_smooth[[i]]), 2)
  sd <- round(sd(summary_LWC_herd2_smooth[[i]]), 2)
  print(data.frame(stat = i, avg, sd))
}

# Herd 3 - raw data
dailyavg_herd3_raw <- dailywts_herd3_raw %>%
  group_by(RFID, Date) %>%
  summarise(dailyavg_raw = round(mean(dailywt), 2)) %>%
  ungroup()

dailyavg_herd3_raw_merged <- merge(dailywts_herd3_new, dailyavg_herd3_raw, by = c("RFID", "Date"), all.x = TRUE)

dailyavg_herd3_raw_splited <- split(dailyavg_herd3_raw_merged, f = dailyavg_herd3_raw_merged$RFID)

dailyavg_herd3_raw_list <- list()

for (i in seq_along(dailyavg_herd3_raw_splited)) {
  dailyavg_herd3_raw_list[[i]] <- with(dailyavg_herd3_raw_splited[[i]], approx(x = Date, y = dailyavg_raw, xout = dates_herd3)$y)
}

dailyavg_herd3_raw_est <- cbind(dailyavg_herd3_raw_merged, est_dailywt = unlist(dailyavg_herd3_raw_list))
dailyavg_herd3_raw_est$est_dailywt <- round(dailyavg_herd3_raw_est$est_dailywt, 2)

LWC_herd3_raw <- dailyavg_herd3_raw_est %>%
  group_by(RFID) %>%
  mutate(daily_lwc = c(NA, diff(est_dailywt))) %>%
  ungroup()


summary_LWC_herd3_raw <- LWC_herd3_raw %>%
  group_by(RFID) %>%
  summarise(mean = round(mean(daily_lwc, na.rm = TRUE), 2),
            min = round(min(daily_lwc, na.rm = TRUE), 2),
            max = round(max(daily_lwc, na.rm = TRUE), 2),
            sd = round(sd(daily_lwc, na.rm = TRUE), 2),
            cv = round(sd(daily_lwc, na.rm = TRUE)/mean(daily_lwc, na.rm = TRUE)*100, 2))

for (i in names(summary_LWC_herd3_raw[, 2:6])) {
  avg <- round(mean(summary_LWC_herd3_raw[[i]]), 2)
  sd <- round(sd(summary_LWC_herd3_raw[[i]]), 2)
  print(data.frame(stat = i, avg, sd))
}

# herd 3 -oulier removed data
LWC_herd3_out <- dailywts_herd3_est %>%
  group_by(RFID) %>%
  mutate(daily_lwc = c(NA, diff(est_dailywt))) %>%
  ungroup()


summary_LWC_herd3_out <- LWC_herd3_out %>%
  group_by(RFID) %>%
  summarise(mean = round(mean(daily_lwc, na.rm = TRUE), 2),
            min = round(min(daily_lwc, na.rm = TRUE), 2),
            max = round(max(daily_lwc, na.rm = TRUE), 2),
            sd = round(sd(daily_lwc, na.rm = TRUE), 2),
            cv = round(sd(daily_lwc, na.rm = TRUE)/mean(daily_lwc, na.rm = TRUE)*100, 2))

for (i in names(summary_LWC_herd3_out[, 2:6])) {
  avg <- round(mean(summary_LWC_herd3_out[[i]]), 2)
  sd <- round(sd(summary_LWC_herd3_out[[i]]), 2)
  print(data.frame(stat = i, avg, sd))
}

# herd 3 - smoothed data
LWC_herd3_smooth <- dailywts_herd3_smooth %>%
  group_by(RFID) %>%
  mutate(daily_lwc = c(NA, diff(roll_avg))) %>%
  ungroup()

summary_LWC_herd3_smooth <- LWC_herd3_smooth %>%
  group_by(RFID) %>%
  summarise(mean = round(mean(daily_lwc, na.rm = TRUE), 2),
            min = round(min(daily_lwc, na.rm = TRUE), 2),
            max = round(max(daily_lwc, na.rm = TRUE), 2),
            sd = round(sd(daily_lwc, na.rm = TRUE), 2),
            cv = round(sd(daily_lwc, na.rm = TRUE)/mean(daily_lwc, na.rm = TRUE)*100, 2))

for (i in names(summary_LWC_herd3_smooth[, 2:6])) {
  avg <- round(mean(summary_LWC_herd3_smooth[[i]]), 2)
  sd <- round(sd(summary_LWC_herd3_smooth[[i]]), 2)
  print(data.frame(stat = i, avg, sd))
}

# plotting
avg_LWC_herd1 <- LWC_herd1_smooth %>%
  group_by(Date) %>%
  summarize(avg_LWC = round(mean(daily_lwc, na.rm = TRUE), 2))

plot_herd1_LWC <- ggplot(avg_LWC_herd1, aes(x = Date, y = avg_LWC)) +
  geom_line() +
  scale_x_date(date_labels = "%m-%d", date_breaks = "10 days") +
  labs(title = "Herd 1", x = "Date (Year 2020 - 2021)", y = "Daily LWC (kg)") +
  theme(plot.title = element_text(hjust = 0.5))

avg_LWC_herd2 <- LWC_herd2_smooth %>%
  group_by(Date) %>%
  summarize(avg_LWC = round(mean(daily_lwc, na.rm = TRUE), 2))

plot_herd2_LWC <- ggplot(avg_LWC_herd2, aes(x = Date, y = avg_LWC)) +
  geom_line() +
  scale_x_date(date_labels = "%m-%d", date_breaks = "15 days") +
  labs(title = "Herd 2", x = "Date (Year 2021 - 2022)", y = "Daily LWC (kg)") +
  theme(plot.title = element_text(hjust = 0.5))

avg_LWC_herd3 <- LWC_herd3_smooth %>%
  group_by(Date) %>%
  summarize(avg_LWC = round(mean(daily_lwc, na.rm = TRUE), 2))

plot_herd3_LWC <- ggplot(avg_LWC_herd3, aes(x = Date, y = avg_LWC)) +
  geom_line() +
  scale_x_date(date_labels = "%m-%d", date_breaks = "30 days") +
  labs(title = "Herd 3", x = "Date (Year 2022 - 2023)", y = "Daily LWC (kg)") +
  theme(plot.title = element_text(hjust = 0.5))

plot_LWC <- grid.arrange(plot_herd1_LWC, plot_herd2_LWC, plot_herd3_LWC, nrow = 1)







