library(DMMongoDB)
library(tidyverse)
library(lme4)
library(lubridate)
library(gridExtra)
library(zoo)
library(DescTools)
library(xgboost)


dailywts_alltime <- read.csv(file = "Belmont_dailywts_alltime.csv")
staticwts_alltime <- read.csv(file = "Belmont_staticwts_alltime.csv")
Belmont_DOB <- read.csv(file = "Belmont_DOB.csv")
weather_alltime <- read.csv(file = "WeatherData_39083.csv")


# changing some column names and removing invalid records
dailywts_alltime <- select(dailywts_alltime, -X)
colnames(dailywts_alltime)[colnames(dailywts_alltime) == "Date"] <- "DateTime"
dailywts_alltime$Date <- as.Date(dailywts_alltime$DateTime, format = "%d-%m-%y")
colnames(dailywts_alltime)[colnames(dailywts_alltime) == "Weight"] <- "dailywt"
dailywts_alltime <- dailywts_alltime[, -c(3:5)]


staticwts_alltime <- select(staticwts_alltime, -X)
colnames(staticwts_alltime)[colnames(staticwts_alltime) == "Weight"] <- "staticwt"
staticwts_alltime$Date <- as.Date(staticwts_alltime$Date)
staticwts_alltime <- staticwts_alltime[staticwts_alltime$Date != "0014-06-20", ]
staticwts_alltime <- staticwts_alltime[, -4]


Belmont_DOB <- select(Belmont_DOB, -c(X, X_id))
Belmont_DOB <- subset(Belmont_DOB, birthDate != "1970-01-01" & sex != "xxxxxx")
Belmont_DOB <- subset(Belmont_DOB, birthWeight > 0)
Belmont_DOB$breed[Belmont_DOB$breed == "Composite "] <- "Composite"
#Belmont_DOB <- Belmont_DOB[-c(2333, 2354), ]



# selecting animals for which birthDate, birthWeight, staticwt and dailywt available

common_RFID <- intersect(unique(dailywts_alltime$RFID), Belmont_DOB$RFID)

DOB_data <- Belmont_DOB %>%
  filter(RFID %in% common_RFID) %>%
  distinct()

dailywts_all <- dailywts_alltime %>%
  filter(RFID %in% common_RFID)

staticwts_all <- staticwts_alltime %>%
  filter(RFID %in% common_RFID)


##########################################

# outlier removal of static weights
staticwts_raw <- subset(staticwts_all, staticwt >= 19)

staticwts_raw <- staticwts_raw %>%
  group_by(RFID) %>%
  mutate(mean_wt = round(mean(staticwt), 2), sd_wt = round(sd(staticwt), 2))


staticwts_out <- data.frame()

for (i in unique(staticwts_raw$RFID)) {
  animal_data <- subset(staticwts_raw, RFID == i)
  
  lower_limit <- animal_data$mean_wt - 1.5 * animal_data$sd_wt
  upper_limit <- animal_data$mean_wt + 1.5 * animal_data$sd_wt
  
  filtered_animal_data <- subset(animal_data, staticwt >= lower_limit & staticwt <= upper_limit)
  staticwts_out <- rbind(staticwts_out, filtered_animal_data)
}


# averaging staticwts if more than one record in a day
staticwts_data <- staticwts_out %>%
  group_by(RFID, Date) %>%
  summarise(staticwt = round(mean(staticwt), 2)) %>%
  ungroup()


# outlier removal of dailywts 

dailywts_raw <- subset(dailywts_all, dailywt >= 19 & dailywt <= 1.5 * max(staticwts_data$staticwt))

dailywts_raw$day <- as.numeric(dailywts_raw$Date - min(dailywts_raw$Date) + 1)

dailywts_raw$day_interval <- cut(dailywts_raw$day, breaks = seq(0, max(dailywts_raw$day) + 7, by = 7), include.lowest = FALSE)


dailywts_raw <- dailywts_raw %>%
  group_by(RFID, day_interval) %>%
  mutate(
    avg_7d = round(mean(dailywt), 2),
    sd_7d = round(sd(dailywt), 2)
  )

dailywts_raw$sd_7d[is.na(dailywts_raw$sd_7d)] <- 0

dailywts_raw$lower_limit <- dailywts_raw$avg_7d - 1.5 * dailywts_raw$sd_7d
dailywts_raw$upper_limit <- dailywts_raw$avg_7d + 1.5 * dailywts_raw$sd_7d


dailywts_out <- subset(dailywts_raw, dailywt >= lower_limit & dailywt <= upper_limit)

# ggplot(dailywts_out, aes(x = day, y = dailywt, color = RFID)) +
#   geom_point() + guides(color = "none")

# averaging dailywts if more than one record in a day
dailywts_data <- dailywts_out %>%
  group_by(RFID, Date) %>%
  summarise(dailywt = round(mean(dailywt), 2)) %>%
  ungroup()

dailywts_data <- dailywts_data %>%
  group_by(RFID) %>%
  filter(n_distinct(dailywt) >=2)



# approach to find optimum window size for rolling average for smoothing of wow ####


#selecting static weights between 2023-03-03 to 2023-03-31 with at least two weight records
staticwts_rollavg <- subset(staticwts_data, Date >= "2023-03-03" & Date <= "2023-03-31")

staticwts_rollavg <- staticwts_rollavg %>%
  group_by(RFID) %>%
  filter(n_distinct(staticwt) >=2)

# ggplot(staticwts_rollavg, aes(Date, y = staticwt)) +
#   geom_point()


# selecting wow between 2023-02-21 to 2023-04-10 (10 day prior and 10 days after of static weight days) for animals selected

dailywts_rollavg <- subset(dailywts_data, Date >= "2023-02-21" & Date <= "2023-04-10")

dailywts_rollavg <- dailywts_rollavg %>%
  filter(RFID %in% unique(staticwts_rollavg$RFID))

dailywts_rollavg$avg_1DW <- dailywts_rollavg$dailywt

dates_dailywt_rollavg <- seq(min(dailywts_rollavg$Date), max(dailywts_rollavg$Date), by = "day")
new_dailywts_rollavg <- expand.grid(RFID = unique(dailywts_rollavg$RFID), Date = dates_dailywt_rollavg)

merged_dailywts_rollavg <- merge(new_dailywts_rollavg, dailywts_rollavg, by = c("RFID", "Date"), all.x = TRUE)


window_sizes <- 2:20

dailywts_rollavg_list <- list()

for (i in unique(merged_dailywts_rollavg$RFID)) {
  animal_data <- subset(merged_dailywts_rollavg, RFID == i)
  
  rolling_averages <- lapply(window_sizes, function(k) {
    rollmean(animal_data$avg_1DW, k = k, align = 'center', na.pad = TRUE, na.rm = TRUE)
  })
  
  temp_df <- animal_data
  
  for (j in window_sizes) {
    col_name <- paste0("avg_", j, "DW")
    temp_df[col_name] <- unlist(rolling_averages[[j-1]])
  }
  
  numeric_cols <- sapply(temp_df, is.numeric)
  temp_df[numeric_cols] <- round(temp_df[numeric_cols], 2)
  
  dailywts_rollavg_list[[i]] <- temp_df
}

dailywts_roll <- do.call(rbind, dailywts_rollavg_list)


allwts_rollavg <- merge(dailywts_roll, staticwts_rollavg, by = c("RFID", "Date"), all.x =  TRUE)


simulated_data <- allwts_rollavg[, !(names(allwts_rollavg) %in% c("RFID", "Date", "dailywt", "avg_1DW", "staticwt", "est_staticwt"))]

CCC_values <- data.frame(window_size = numeric(), ccc = numeric(), RMSE = numeric())

for (col_name in names(simulated_data)) {
  ccc_result <- CCC(simulated_data[[col_name]], allwts_rollavg$staticwt, ci = "z-transform", conf.level = 0.95, na.rm = TRUE)
  
  squared_diff <- (simulated_data[[col_name]] - allwts_rollavg$staticwt)^2
  RMSE <- sqrt(mean(squared_diff, na.rm = TRUE))
  
  CCC_values <- rbind(CCC_values, data.frame(window = col_name, ccc = ccc_result$rho.c, RMSE = RMSE))
}

CCC_values <- data.frame(window_size = 2:20, ccc = CCC_values$ccc.est, 3, RMSE = CCC_values$RMSE)


plot1 <- ggplot(CCC_values, aes(x = window_size, y = ccc)) +
  geom_line() + geom_point() + xlab("Window size") + ylab("CCC") +
  scale_x_continuous(breaks = seq(2, 30, by = 4)) +
  theme(panel.grid.minor = element_blank())

plot2 <- ggplot(CCC_values, aes(x = window_size, y = RMSE)) +
  geom_line() + geom_point() + xlab("Window size") + ylab("RMSE") +
  scale_x_continuous(breaks = seq(2, 30, by = 4)) +
  theme(panel.grid.minor = element_blank())

grid.arrange(plot1, plot2, nrow = 1)



# using 14 days rolling average to smooth the dailywts_data (max CCC and least RMSE) ####

dates_dailywt_smooth <- seq(min(as.Date(dailywts_data$Date)), max(as.Date(dailywts_data$Date)), by = "day")
new_dailywts_smooth <- expand.grid(RFID = unique(dailywts_data$RFID), Date = dates_dailywt_smooth)

merged_dailywts_smooth <- merge(new_dailywts_smooth, dailywts_data, by = c("RFID", "Date"), all.x = TRUE)

dailywts_smooth <- list()

for (i in unique(merged_dailywts_smooth$RFID)) {
  animal_data <- subset(merged_dailywts_smooth, RFID == i)
  
  smooth_dailywt <- rollapply(animal_data$dailywt, width = 14, align = 'center', FUN = mean, fill = NA, na.rm = TRUE)
  
  rollavg_animal_data <- cbind(animal_data, smooth_dailywt)
  
  dailywts_smooth <- rbind(dailywts_smooth, rollavg_animal_data)
}

dailywts_smooth$smooth_dailywt <- round(dailywts_smooth$smooth_dailywt)

dailywts_smooth <- subset(dailywts_smooth, !is.na(dailywt))

# plotting outlier removed and smoothed weights for example
# example_data <- subset(dailywts_smooth, RFID == "942 000020942890")
# 
# ggplot(example_data, aes(x = Date)) +
#   geom_line(aes(y = smooth_dailywt, color = "Smoothed")) +
#   geom_line(aes(y = dailywt, color = "Outlier removed"))


complete_data <- merge(dailywts_smooth, DOB_data, by = "RFID", all.x = TRUE)


# simulation model development ## 
complete_data <- complete_data %>%
  arrange(RFID, Date) %>%
  group_by(RFID) %>%
  mutate(daily_growth = c(NA, diff(smooth_dailywt)))

complete_data <- subset(complete_data, !is.na(smooth_dailywt) | !is.nan(smooth_dailywt))


complete_data <- mutate(complete_data, season = case_when(
  as.numeric(format(Date, "%m")) %in% c(12, 01, 02) ~ "Summer",
  as.numeric(format(Date, "%m")) %in% c(03, 04, 05)   ~ "Autumn",
  as.numeric(format(Date, "%m")) %in% c(06, 07, 08)   ~ "Winter",
  as.numeric(format(Date, "%m")) %in% c(09, 10, 11) ~ "Spring"
))


growth_rates <- complete_data %>%
  group_by(season, sex) %>%
  summarise(avg_growth = round(mean(daily_growth, na.rm = TRUE), 2),
            sd_growth = round(sd(daily_growth, na.rm = TRUE), 2))

growth_rates



sim_model <- function(n_males, n_females, n_days, min_birthDate, max_birthDate) {
  
  n_animals <- n_males + n_females
  birth_dates <- sample(seq(as.Date(min_birthDate), as.Date(max_birthDate), by = "days"), 
                        n_animals, replace = TRUE)
  birth_weights <- rnorm(n_animals, mean = 38, sd = 6)
  
  data_animals <- data.frame(
    ID = 1:n_animals,
    sex = c(rep("male", each = n_males), rep("female", each = n_females)),
    birthDate = birth_dates,
    birthWeight = birth_weights
  )
  
  simulation_dates <- seq(min(birth_dates), max(birth_dates) + n_days, by = "days")
  
  data_new <- expand.grid(ID = unique(data_animals$ID), simDate = simulation_dates)
  
  data_merged <- merge(data_new, data_animals, by = "ID", all.x = TRUE)
  
  data_merged <- mutate(data_merged, season = case_when(
    as.numeric(format(simDate, "%m")) %in% c(12, 01, 02) ~ "Summer",
    as.numeric(format(simDate, "%m")) %in% c(03, 04, 05)   ~ "Autumn",
    as.numeric(format(simDate, "%m")) %in% c(06, 07, 08)   ~ "Winter",
    as.numeric(format(simDate, "%m")) %in% c(09, 10, 11) ~ "Spring"
  ))
  
  data_merged <- data_merged %>%
    mutate(
      growth_rate = case_when(
        season == "Summer" & sex == "male" ~ rnorm(n(), 0.15, 21.5),
        season == "Summer" & sex == "female" ~ rnorm(n(), 0.46, 15.4),
        season == "Autumn" & sex == "male" ~ rnorm(n(), 0.27, 30),
        season == "Autumn" & sex == "female" ~ rnorm(n(), 0.1, 17.1),
        season == "Winter" & sex == "male" ~ rnorm(n(), 0.8, 14.6),
        season == "Winter" & sex == "female" ~ rnorm(n(), 0.33, 11.3),
        season == "Spring" & sex == "male" ~ rnorm(n(), -0.54, 26.9),
        season == "Spring" & sex == "female" ~ rnorm(n(), 0.06, 12.6)))
  
  data_merged$age <- as.numeric(data_merged$simDate - data_merged$birthDate + 1)
  data_sim <- subset(data_merged, age > 0)
  
  
  data_sim <- data_sim %>%
    arrange(ID, simDate) %>%
    group_by(ID) %>%
    mutate(simWeight = birthWeight + cumsum(growth_rate))
  
  return(data_sim)
}


# simulation run
n_males <- 50
n_females <- 50
n_days <- 700
min_birthDate <- "2020-09-01"
max_birthDate <- "2021-02-28"

sim_result <- sim_model(n_males, n_females, n_days, min_birthDate, max_birthDate)


ggplot(sim_result, aes(x = simDate, y = simWeight, color = factor(ID))) +
  geom_line() +
  guides(color = "none")

ggplot(sim_result, aes(x = age, y = simWeight, color = factor(ID))) +
  geom_line() +
  guides(color = "none")




















