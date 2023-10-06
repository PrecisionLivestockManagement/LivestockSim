library(DMMongoDB)
library(tidyverse)
library(lme4)
library(lubridate)
library(gridExtra)
library(zoo)
library(DescTools)


dailywts_alltime <- read.csv(file = "Data/Belmont_dailywts_alltime.csv")
staticwts_alltime <- read.csv(file = "Data/Belmont_staticwts_alltime.csv")
Belmont_DOB <- read.csv(file = "Data/Belmont_DOB.csv")
weather_alltime <- read.csv(file = "Data/WeatherData_39083.csv")


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
Belmont_DOB <- subset(Belmont_DOB, birthDate != "1970-01-01" & birthDate != "2023-01-01" & breed != "xxxxxx")
Belmont_DOB <- subset(Belmont_DOB, birthWeight > 0)
Belmont_DOB$breed[Belmont_DOB$breed == "Composite "] <- "Composite"

weather_alltime <- weather_alltime[, c("YYYY.MM.DD", "daily_rain", "max_temp", "min_temp")]
colnames(weather_alltime)[colnames(weather_alltime) == "YYYY.MM.DD"] <- 'Date'
weather_alltime$Date <- as.Date(weather_alltime$Date)

# selecting required weather data and taking monthly average of rainfall and temp.
weather_data <- subset(weather_alltime, Date >= as.Date("2015-01-01") & Date <= as.Date("2023-12-31"))

weather_data$avg_temp <- round((weather_data$max_temp + weather_data$min_temp) / 2, 1)

weather_data <- weather_data %>%
  group_by(month = format(Date, "%Y-%m")) %>%
  mutate(monthly_rain = round(mean(daily_rain), 2))

weather_data <- weather_data %>%
  group_by(month = format(Date, "%Y-%m")) %>%
  mutate(rain_sd = round(sd(daily_rain), 2))

# Calculate monthly average temperature and add it as a new column in 'weather_data'
weather_data <- weather_data %>%
  group_by(month = format(Date, "%Y-%m")) %>%
  mutate(monthly_temp = round(mean(avg_temp), 2))

weather_data <- weather_data %>%
  group_by(month = format(Date, "%Y-%m")) %>%
  mutate(temp_sd = round(sd(avg_temp), 2)) %>%
  ungroup()

weather_data_monthly <- weather_data[, -c(2:6)]




# example_data <- dailywts_DOB_raw %>%
#   group_by(age) %>%
#   summarise(avg_weight = mean(dailywt))
# 
# ggplot(example_data, aes(x = age, y = avg_weight)) +
#   geom_point() +
#   geom_smooth()


# selecting animals for which birthDate, birthWeight, staticwt and dailywt available

common_RFID <- intersect(intersect(unique(staticwts_alltime$RFID), unique(dailywts_alltime$RFID)), Belmont_DOB$RFID)

DOB_data <- Belmont_DOB %>%
  filter(RFID %in% common_RFID) %>%
  distinct()

dailywts_raw <- dailywts_alltime %>%
  filter(RFID %in% common_RFID)

staticwts_raw <- staticwts_alltime %>%
  filter(RFID %in% common_RFID)


# outlier removal of static weights
staticwts <- subset(staticwts_raw, staticwt >= min(DOB_data$birthWeight))

staticwts <- staticwts %>%
  group_by(RFID) %>%
  mutate(mean_staticwt = round(mean(staticwt), 2), sd_staticwt = round(sd(staticwt), 2))


staticwts_out <- data.frame()

for (i in unique(staticwts$RFID)) {
  animal_data <- subset(staticwts, RFID == i)
  
  lower_limit <- animal_data$mean_staticwt - 1.5 * animal_data$sd_staticwt
  upper_limit <- animal_data$mean_staticwt + 1.5 * animal_data$sd_staticwt
  
  filtered_animal_data <- subset(animal_data, staticwt >= lower_limit & staticwt <= upper_limit)
  staticwts_out <- rbind(staticwts_out, filtered_animal_data)
}

staticwts_out <- staticwts_out[, -c(4, 5)]

# averaging if more than 1 record in a single day
staticwts_data <- staticwts_out %>%
  group_by(RFID, Date) %>%
  summarise(staticwt = round(mean(staticwt), 2)) %>%
  ungroup()
  
  
  
  # distinct(RFID, Date, .keep_all = TRUE)

# selecting animals with at least 2 staticwts record
# staticwts_out <- staticwts_out %>%
#   group_by(RFID) %>%
#   filter(n_distinct(Date) >= 2) %>%
#   ungroup()


# outlier removal of dailywts
dailywts <- subset(dailywts_raw, dailywt >= min(DOB_data$birthWeight) & dailywt <= 2 * max(staticwts_out$staticwt))

# dailywts <- dailywts %>%
#   group_by(RFID, Date) %>%
#   summarise(dailywt = round(mean(dailywt), 2)) %>%
#   ungroup()

# dailywts_DOB_raw <- merge(dailywts_out, DOB_data, by = "RFID")
# dailywts_DOB_raw$age <- as.numeric(as.Date(dailywts_DOB_raw$Date) - as.Date(dailywts_DOB_raw$birthDate) + 1)
# 
# dailywts_DOB_raw <- dailywts_DOB_raw %>%
#   mutate(dailywt = ifelse(age == 1, birthWeight, dailywt))

# dailywts_DOB_raw <- dailywts_DOB_raw %>%
#   group_by(RFID) %>%
#   mutate(
#     mean_dailywt = round(mean(dailywt), 2),
#     sd_dailywt = round(sd(dailywt), 2)
#   )

# dailywts_DOB_raw <- dailywts_DOB_raw %>%
#   group_by(RFID) %>%
#   mutate(mean_dailywt = round(mean(dailywt[age >= 201]), 2), sd_dailywt = round(sd(dailywt[age >= 201]), 2))


# average_dailywt <- mean(data$dailywt[data$age >= 1 & data$age <= 200])

dailywts <- dailywts %>%
  group_by(RFID) %>%
  mutate(mean_dailywt = round(mean(dailywt), 2), sd_dailywt = round(sd(dailywt), 2))

dailywts_out <- data.frame()

for (i in unique(dailywts$RFID)) {
  animal_data <- subset(dailywts, RFID == i)
  
  lower_limit <- animal_data$mean_dailywt - 1.5 * animal_data$sd_dailywt
  upper_limit <- animal_data$mean_dailywt + 1.5 * animal_data$sd_dailywt
  
  filtered_animal_data <- subset(animal_data, dailywt >= lower_limit & dailywt <= upper_limit)
  dailywts_out <- rbind(dailywts_out, filtered_animal_data)
}

dailywts_out <- dailywts_out[, -c(4, 5)]

# averaging dailywts if more than one record in a day
dailywts_data <- dailywts_out %>%
  group_by(RFID, Date) %>%
  summarise(dailywt = round(mean(dailywt), 2)) %>%
  ungroup()

# selecting animals with at least 2 days of dailywts record
dailywts_data <- dailywts_data %>%
  group_by(RFID) %>%
  filter(n_distinct(Date) >= 2) %>%
  ungroup()


# approach to find optimum window size for rolling average for smoothing of wow ####
#selecting static weights between 2023-03-03 to 2023-03-31
staticwts_rollavg <- subset(staticwts_data, Date >= "2023-03-03" & Date <= "2023-03-31")

staticwts_rollavg <- staticwts_rollavg %>%
  group_by(RFID) %>%
  filter(n_distinct(Date) >= 2) %>%
  ungroup()

ggplot(staticwts_rollavg, aes(Date, y = staticwt)) +
  geom_point()


# linear interpolation to get the static weights for the missing days
dates_staticwt_rollavg <- seq(as.Date("2023-03-03"), as.Date("2023-03-31"), by = "day")
new_staticwts_rollavg <- expand.grid(RFID = unique(staticwts_rollavg$RFID), Date = dates_staticwt_rollavg)

merged_staticwts_rollavg <- merge(new_staticwts_rollavg, staticwts_rollavg, by = c("RFID", "Date"), all.x = TRUE)

staticwts_splited_rollavg <- split(merged_staticwts_rollavg, f = merged_staticwts_rollavg$RFID)

est_staticwts_rollavg <- list()

for (i in seq_along(staticwts_splited_rollavg)) {
  est_staticwts_rollavg[[i]] <- with(staticwts_splited_rollavg[[i]], approx(x = Date, y = staticwt, xout = dates_staticwt_rollavg)$y)
}

staticwts_estimated <- cbind(merged_staticwts_rollavg, est_staticwts = unlist(est_staticwts_rollavg))
staticwts_estimated$est_staticwts <- round(staticwts_estimated$est_staticwts, 2)


# selecting wow between 2023-02-16 to 2023-04-15 (10 day prior and 10 days after of static weight days) for animals selected
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

dailywts_rollavg <- do.call(rbind, dailywts_rollavg_list)


allwts_rollavg <- merge(dailywts_rollavg, staticwts_estimated, by = c("RFID", "Date"))


simulated_data <- allwts_rollavg[, !(names(allwts_rollavg) %in% c("RFID", "Date", "dailywt", "staticwt", "est_staticwts"))]

CCC_values <- data.frame(window_size = numeric(), ccc = numeric(), RMSE = numeric(), rsq = numeric())

for (col_name in names(simulated_data)) {
  ccc_result <- CCC(simulated_data[[col_name]], allwts_rollavg$staticwt, ci = "z-transform", conf.level = 0.95, na.rm = TRUE)
  
  squared_diff <- (simulated_data[[col_name]] - allwts_rollavg$staticwt)^2
  RMSE <- sqrt(mean(squared_diff, na.rm = TRUE))
  
  data <- data.frame(simulated_data[[col_name]], allwts_rollavg$staticwt)
  model <- lm(allwts_rollavg$staticwt ~ simulated_data[[col_name]], data = data)
  rsq <- summary(model)$r.squared
  
  CCC_values <- rbind(CCC_values, data.frame(window = col_name, ccc = ccc_result$rho.c, RMSE = RMSE, rsq = rsq))
}

CCC_values <- data.frame(window_size = 1:20, ccc = CCC_values$ccc.est, RMSE = CCC_values$RMSE, rsq = CCC_values$rsq)


plot1 <- ggplot(CCC_values, aes(x = window_size, y = ccc)) +
  geom_line() + geom_point() + xlab("Window size") + ylab("CCC")

plot2 <- ggplot(CCC_values, aes(x = window_size, y = RMSE)) +
  geom_line() + geom_point() + xlab("Window size")

grid.arrange(plot1, plot2, nrow = 1)


# using 11 days rolling average to smooth the dailywts_data (max CCC and least RMSE) ####

dates_dailywt <- seq(min(as.Date(DOB_data$birthDate)), max(as.Date(dailywts_data$Date)), by = "day")
new_dailywts <- expand.grid(RFID = unique(dailywts_data$RFID), Date = dates_dailywt)

merged_dailywts <- merge(new_dailywts, dailywts_data, by = c("RFID", "Date"), all.x = TRUE)

dailywts_rollavg <- list()

for (i in unique(merged_dailywts$RFID)) {
  animal_data <- subset(merged_dailywts, RFID == i)
  
  smooth_dailywt <- rollmean(animal_data$dailywt, k = 11, align = 'center', na.pad = TRUE, na.rm = TRUE)
  
  rollavg_animal_data <- cbind(animal_data, smooth_dailywt)
  
  dailywts_rollavg <- rbind(dailywts_rollavg, rollavg_animal_data)
}

dailywts_rollavg$smooth_dailywt <- round(dailywts_rollavg$smooth_dailywt, 2)
dailywts_rollavg$smooth_dailywt[is.nan(dailywts_rollavg$smooth_dailywt)] <- NA


# dailywts_smooth <- dailywts_rollavg[!is.na(dailywts_rollavg$smooth_dailywt) & !is.nan(dailywts_rollavg$smooth_dailywt), c("RFID", "Date", "smooth_dailywt")]
# dailywts_smooth$smooth_dailywt[is.nan(dailywts_smooth$smooth_dailywt)] <- NA


# selecting animals with at least 100 days of dailywts record available after smooting
# animals_selected <- subset(dailywts_count, count >= 350)
# 
# 
# dailywts_selected <- dailywts_smooth %>%
#   filter(RFID %in% animals_selected$RFID)

# dailywts_DOB_data <- dailywts_DOB_data %>%
#   group_by(age) %>%
#   mutate(mean_weight = mean(smooth_dailywt)) %>%
#   ungroup()


# dailywts_count <- data.frame(RFID = character(), count = numeric())
# 
# for (i in unique(dailywts_DOB_data$RFID)) {
#   count <- data.frame(RFID = i, count = sum(dailywts_DOB_data$RFID == i))
#   dailywts_count <- rbind(dailywts_count, count)
# }
# 
# animals_selected <- subset(dailywts_count, count >= 100)
# 
# dailywts_selected <- dailywts_DOB_data %>%
#   filter(RFID %in% animals_selected$RFID)



# example_data <- subset(dailywts_DOB_data, !is.na(smooth_dailywt))
# 
# example_avg <- example_data %>%
#   group_by(age) %>%
#   summarise(weight = mean(smooth_dailywt))
# 
# 
# ggplot(example_avg, aes(x = age, y = weight)) +
#   geom_point()




# selecting animals for which wow recorded at least once every 200 days

# dailywts_DOB_data <- merge(dailywts_smooth, DOB_data, by = "RFID")
# dailywts_DOB_data$age <- as.numeric(as.Date(dailywts_DOB_data$Date) - as.Date(dailywts_DOB_data$birthDate) + 1)
# 
# 
# dailywts_DOB_data <- dailywts_DOB_data %>%
#   filter(!is.na(smooth_dailywt)) %>%
#   mutate(AgeGroup = cut(age, breaks = c(0, 200, 400, 600), labels = c("0-200", "201-400", "401-600")))
# 
# selected_animals <- dailywts_DOB_data %>%
#   group_by(RFID) %>%
#   summarise(NumDistinctAgeGroups = n_distinct(AgeGroup)) %>%
#   filter(NumDistinctAgeGroups == 3)

 
# # merging daily weights and DOB data and grouping 0-200, 201-400, 401-600 and 601-800 age groups 
# dailywts_DOB <- merge(dailywts_raw, Belmont_DOB, by = "RFID")
# dailywts_DOB$age <- as.numeric(as.Date(dailywts_DOB$Date) - as.Date(dailywts_DOB$birthDate) + 1)
# 
# dailywts_DOB <- dailywts_DOB %>%
#   mutate(AgeGroup = cut(age, breaks = c(0, 200, 400, 600), labels = c("0-200", "201-400", "401-600")))
# 
# selected_dailywts <- dailywts_DOB %>%
#   group_by(RFID) %>%
#   summarise(NumDistinctAgeGroups = n_distinct(AgeGroup)) %>%
#   filter(NumDistinctAgeGroups == 4)
# 
# 
# # Join the selected_animals back with the original data frame to get the complete rows
# staticwts_selected <- staticwts_DOB %>%
#   semi_join(selected_animals, by = "RFID")
# 
# staticwts_selected <- subset(staticwts_selected, !is.na(AgeGroup))
# 
# dailywts_selected <- dailywts_raw %>%
#   filter(RFID %in% unique(staticwts_selected$RFID))
# 
# 
# 
# selected_animals <- unique(subset(dailywts_DOB_data, age >= 900)$RFID)
# 
# dailywts_raw <- dailywts_DOB_data %>%
#   filter(RFID %in% selected_animals)
# 
# staticwts_raw <- staticwts_alltime %>%
#   filter(RFID %in% selected_animals)



# example_data <- subset(dailywts_DOB_data, !is.na(smooth_dailywt))
# example_data1 <- subset(example_data, age > 1250)
# length(unique(example_data1$RFID))
# 
# dailywts_count <- data.frame(RFID = character(), count = numeric())
# 
# for (i in unique(example_data$RFID)) {
#   count <- data.frame(RFID = i, count = sum(example_data$RFID == i))
#   dailywts_count <- rbind(dailywts_count, count)
# }

# dailywts_count <- dailywts_count %>%
#   filter(count >= 100)

# 
# #dailywts_count <- dailywts_count[order(-dailywts_count$count), ]
# 
# staticwts_count <- data.frame(RFID = character(), count = numeric())
# 
# for (i in unique(staticwts_out$RFID)) {
#    count <- data.frame(RFID = i, count = sum(staticwts_out$RFID == i))
#    staticwts_count <- rbind(staticwts_count, count)
#    }
#
# staticwts_count <- staticwts_count[order(-staticwts_count$count), ]


# merging dailywts and DOB, staticwts and weather data
dailywts_DOB_data <- merge(dailywts_rollavg, DOB_data, by = "RFID")
dailywts_DOB_data$age <- as.numeric(as.Date(dailywts_DOB_data$Date) - as.Date(dailywts_DOB_data$birthDate) + 1)

dailywts_DOB_data <- subset(dailywts_DOB_data, age >= 1 & age <= 1500)

dailywts_DOB_data <- dailywts_DOB_data %>%
  mutate(smooth_dailywt = ifelse(age == 1, birthWeight, smooth_dailywt))

dailywts_staticwts_data <- merge(dailywts_DOB_data, staticwts_data, by = c("RFID", "Date"), all.x = TRUE, all.y = TRUE)

dailywts_staticwts_data <- dailywts_staticwts_data %>%
  mutate(staticwt = ifelse(age == 1, birthWeight, staticwt))

complete_data <- merge(dailywts_staticwts_data, weather_data_monthly, by = "Date", all.x = TRUE)

complete_data$birthYear <- year(as.Date(complete_data$birthDate))

# complete_data <- subset(complete_data, !is.na(staticwt))

# complete_data_selected <- subset(complete_data, breed == "Composite" | breed == "Brahman")


# complete_data_selected <- complete_data_selected %>%
#   mutate(AgeGroup = cut(age, breaks = c(0, 200, 500), labels = c("0-200", "201-500")))
# 
# selected_animals <- complete_data_selected %>%
#   group_by(RFID) %>%
#   summarise(NumDistinctAgeGroups = n_distinct(AgeGroup)) %>%
#   filter(NumDistinctAgeGroups == 2)


# example_data <- subset(complete_data, breed == "BrahxComp" | breed == "Belmont Red")
# 
# ggplot(complete_data_selected, aes(x = age, y = smooth_dailywt)) +
#   geom_point() +
#   geom_smooth()
# 
# 

# example_data <- subset(dailywts_DOB_data, !is.na(smooth_dailywt))
# 
# ggplot(example_data, aes(x = age)) +
#   geom_histogram()
# 
# 
# ggplot(complete_data_selected, aes(x = age, y = staticwt, color = RFID)) +
#   geom_line() +
#   guides(color = "none")
# 
# example_data1 <- example_data %>%
#   group_by(age) %>%
#   summarize(avg = mean(smooth_dailywt))
# 
# ggplot(example_data1, aes(x = age, y = avg))+
#   geom_point() +
#   geom_line()
# 
# example_data2<- complete_data %>%
#   group_by(age) %>%
#   summarize(avg = mean(staticwt, na.rm = TRUE))
# 
# example_data2 <- subset(example_data2, !is.nan(avg))
# 
# ggplot(example_data2, aes(x = age, y = avg)) +
#   geom_point() +
#   geom_line()

# counting no. of dailywts records and selections RFID with 100+ records
# complete_subset <- subset(complete_data_selected, !is.na(smooth_dailywt))
# 
# subset_200below <- subset(complete_subset, age <= 200)
# subset_200above <- subset(complete_subset, age >= 200)
# subset_600above <- subset(complete_subset, age > 600)
# subset_1200above <- subset(complete_data, age > 1200)
# 
# 
# length(unique(subset_1200above$RFID))


# 
# subset_1200plus <- dailywts_DOB_data %>%
#   filter(age > 1200) %>%
#   length(unique(RFID))
# 
# selected_RFID <- intersect(unique(subset_200below$RFID), unique(subset_200above$RFID))


# complete_avg <- complete_subset %>%
#   group_by(age) %>%
#   summarise(avg_weight = mean(smooth_dailywt))

complete_dailywts <- subset(complete_data, !is.na(smooth_dailywt))

dailywts_count <- data.frame(RFID = character(), count = numeric())

for (i in unique(complete_dailywts$RFID)) {
  count <- data.frame(RFID = i, count = sum(complete_dailywts$RFID == i))
  dailywts_count <- rbind(dailywts_count, count)
}

dailywts_count <- dailywts_count %>%
  filter(count >= 2)

# dailywts_count <- dailywts_count[order(-dailywts_count$count), ]
# dailywts_count <- dailywts_count[1:500, ]


complete_staticwts <- subset(complete_data, !is.na(staticwt))

staticwts_count <- data.frame(RFID = character(), count = numeric())

for (i in unique(complete_staticwts$RFID)) {
  count <- data.frame(RFID = i, count = sum(complete_staticwts$RFID == i))
  staticwts_count <- rbind(staticwts_count, count)
}

staticwts_count <- staticwts_count %>%
  filter(count >= 2)

# selected_dailywt <- subset(complete_data, age >= 450 & !is.na(smooth_dailywt))
# selected_staticwt <- subset(complete_data, age >= 450 & !is.na(staticwt))


# splitting data for model development approx. (70%) and validation (30%)
# set.seed(100)

selected_RFID <- intersect(dailywts_count$RFID, staticwts_count$RFID)

# selected_RFID <- as.character(unique(complete_data$RFID))

# selected_RFID <- dailywts_count$RFID

# model_animals <- sample(selected_RFID, size = 460, replace = FALSE)
# 
# validation_animals <- setdiff(selected_RFID, model_animals)

data_model <- complete_data %>%
  filter(RFID %in% selected_RFID)

# data_validation <- complete_data %>%
#   filter(RFID %in% validation_animals)


# interpolation to get the missing weights for model development, using birthWeight as a day 1 weight

# data_model <- data_model %>%
#   mutate(smooth_dailywt = ifelse(age == 1, birthWeight, smooth_dailywt))


# data_model <- data_model %>%
#   group_by(RFID) %>%
#   mutate(est_dailywt = approx(x = age, y = smooth_dailywt, xout = age)$y)
# 
# data_model$est_dailywt <- round(data_model$est_dailywt, 2)
# 
# data_validation <- data_validation %>%
#   group_by(RFID) %>%
#   mutate(est_dailywt = approx(x = age, y = smooth_dailywt, xout = age)$y)
# 
# data_validation$est_dailywt <- round(data_validation$est_dailywt, 2)
# 
# 
# data_model <- data_model %>%
#   group_by(RFID) %>%
#   mutate(staticwt = approx(x = age, y = staticwt, xout = age)$y)
# 
# data_model$staticwt <- round(data_model$staticwt, 2)
# 
# 
# data_validation <- data_validation %>%
#   group_by(RFID) %>%
#   mutate(staticwt = approx(x = age, y = staticwt, xout = age)$y)
# 
# data_validation$staticwt <- round(data_validation$staticwt, 2)


# data_model <- data_model %>%
#   mutate(est_dailywt = ifelse(is.na(est_dailywt), max(est_dailywt, na.rm = TRUE), est_dailywt))
# 
# data_validation <- data_validation %>%
#   mutate(est_dailywt = ifelse(is.na(est_dailywt), max(est_dailywt, na.rm = TRUE), est_dailywt))
# 
# 
# data_model <- data_model %>%
#   mutate(ref_weight = ifelse(is.na(ref_weight), max(ref_weight), ref_weight))
# 
# data_validation <- data_validation %>%
#   mutate(ref_weight = ifelse(is.na(ref_weight), max(ref_weight), ref_weight))


# data_model <- data_model %>%
#   group_by(RFID) %>%
#   mutate(max_weight = mean(staticwt, na.rm = TRUE))
# 
# 
# data_validation <- data_validation %>%
#   group_by(RFID) %>%
#   mutate(max_weight = mean(staticwt, na.rm = TRUE))

# ggplot(data_model, aes(x = age, y = staticwt, color = RFID)) +
#   geom_point() +
#   guides(color = "none")
# 
# 
# data_model_preweaning <- data_model %>%
#   group_by(RFID) %>%
#   filter(age <= 200) %>%
#   mutate(ref_weight = mean(staticwt, na.rm = TRUE))
# 
# data_model_postweaning <- data_model %>%
#   group_by(RFID) %>%
#   filter(age > 200) %>%
#   mutate(ref_weight = mean(staticwt, na.rm = TRUE))
# 
# data_model <- rbind(data_model_preweaning, data_model_postweaning)
# data_model$ref_weight <- round(data_model$ref_weight, 2)
# 
# 
# 
# data_validation_preweaning <- data_validation %>%
#   group_by(RFID) %>%
#   filter(age <= 200) %>%
#   mutate(ref_weight = mean(staticwt, na.rm = TRUE))
# 
# data_validation_postweaning <- data_validation %>%
#   group_by(RFID) %>%
#   filter(age > 200) %>%
#   mutate(ref_weight = mean(staticwt, na.rm = TRUE))
# 
# data_validation <- rbind(data_validation_preweaning, data_validation_postweaning)
# data_validation$ref_weight <- round(data_validation$ref_weight, 2)



data_model$breed[data_model$breed == "Brahman"] <- 1
data_model$breed[data_model$breed == "Composite"] <- 2
data_model$breed[data_model$breed == "BrahxComp"] <- 3
data_model$breed[data_model$breed == "Belmont Red"] <- 4

data_model$sex[data_model$sex == "female"] <- 0
data_model$sex[data_model$sex == "male"] <- 1


# data_validation$breed[data_validation$breed == "Brahman"] <- 1
# data_validation$breed[data_validation$breed == "Composite"] <- 2
# data_validation$breed[data_validation$breed == "BrahxComp"] <- 3
# data_validation$breed[data_validation$breed == "Belmont Red"] <- 4
# 
# data_validation$sex[data_validation$sex == "female"] <- 0
# data_validation$sex[data_validation$sex == "male"] <- 1

# data_model <- subset(data_model, !is.na(smooth_dailywt))
# data_validation <- subset(data_validation, !is.na(smooth_dailywt))

# model 1 (multiple linear regression) ####
model_1 <- lm(smooth_dailywt ~ age + sex + breed + birthWeight, data = data_model)

summary(model_1)
AIC(model_1)
BIC(model_1)

fixed_effects <- coef(model_1)
# random_effects <- ranef(model_1)

b0 <- fixed_effects["(Intercept)"]
b1 <- fixed_effects["age"]
b2 <- fixed_effects["sex1"]
b3 <- fixed_effects["breed2"]
b4 <- fixed_effects["breed3"]
b5 <- fixed_effects["breed4"]
b6 <- fixed_effects["birthWeight"]
b7 <- fixed_effects["monthly_rain:monthly_temp"]
b8 <- fixed_effects["monthly_rain"]
b9 <- fixed_effects["monthly_temp"]


# vlidation 

data_predicted <- data_model %>%
  group_by(RFID) %>%
  mutate(
    predicted_dailywt = b0 + b1 * age + b2 * as.numeric(sex == 1) +
      b3 * as.numeric(breed == 2) + b4 * as.numeric(breed == 3) + b5 * as.numeric(breed == 4) +
      b6 * birthWeight + b8 * monthly_rain + b9 * monthly_temp +
      rnorm(n(), mean = 0, sd = summary(model_1)$sigma)
  ) %>%
  ungroup()

data_predicted$predicted_dailywt <- round(data_predicted$predicted_dailywt, 2)

# data_predicted <- subset(data_predicted, !is.na(ref_weight) & !is.nan(ref_weight))

data_predicted$actual_diff <- data_predicted$predicted_dailywt - data_predicted$smooth_dailywt

squared_diff <- (data_predicted$predicted_dailywt - data_predicted$smooth_dailywt)^2
RMSE <- sqrt(mean(squared_diff, na.rm = TRUE))
RMSE

model_rsq <- lm(smooth_dailywt ~ predicted_dailywt, data = data_predicted)
rsq <- summary(model_rsq)$r.squared
rsq


# model 2 (brody's plus multiple regression) ####
# brody (non-linear) model

brody_data <- complete_data %>%
  filter(RFID %in% selected_RFID)

Brahman_male <- subset(brody_data, breed == 1 & sex == 1)
Brahman_female <- subset(brody_data, breed == 1 & sex == 0)

Composite_male <- subset(brody_data, breed == 2 & sex == 1)
composite_female <- subset(brody_data, breed == 2 & sex == 0)

BrahxComp_male <- subset(brody_data, breed == 3 & sex == 1)
BrahxComp_female <- subset(brody_data, breed == 3 & sex == 0)

BelmontRed_male <- subset(brody_data, breed == 4 & sex == 1)
BelmontRed_female <- subset(brody_data, breed == 4 & sex == 0)


Brahman_male_model <- nls(smooth_dailywt ~ A * (1 - B * exp(-k * age)), data = Brahman_male, 
                          start = list(A = 600, B = 100, k = 0.01))

summary(Brahman_male_model)


brody_predicted <- Brahman_male
brody_predicted$predictions <- predict(Brahman_male_model, newdata = Brahman_male)

Brahman_female_model <- nls(smooth_dailywt ~ A * (1 - B * exp(-k * age)), data = Brahman_female, 
                            start = list(A = 600, B = 100, k = 0.0001))



Brahman_female_prediction <- predict(Brahman_female_model, newdata = Brahman_female)

brody_predicted <- rbind(brody_predicted, Brahman_female_prediction)


brody_data <- complete_data %>%
  filter(!is.na(smooth_dailywt)) %>%
  mutate(AgeGroup = cut(age, breaks = c(0, 200, 400, 1500), labels = c("0-200", "201-400", "401-1500")))

brody_selected <- brody_data %>%
  group_by(RFID) %>%
  summarise(NumDistinctAgeGroups = n_distinct(AgeGroup)) %>%
  filter(NumDistinctAgeGroups == 3)


brody_data_model <- complete_data %>%
  filter(RFID %in% brody_selected$RFID)

example_data <- subset(brody_data_model, !is.na(smooth_dailywt))



brody_predicted <- data.frame()

for (i in unique(complete_data$RFID)) {
  animal_data <- subset(complete_data, RFID == i)
  
  individual_model <- nls(smooth_dailywt ~ A * (1 - B * exp(-k * age)), data = animal_data, 
                          start = list(A = 60, B = 0.1, k = 0.01))
  
  individual_prediction <- predict(individual_model, new_data = animal_data)
  
  predicted <- merge(animal_data, individual_prediction)
  
  brody_predicted <- rbind(brody_predicted, animal_data)
  
}




example_data <- subset(brody_data_model, RFID == "951 000305782502 ")

brody_model <- nls(smooth_dailywt ~ A * (1 - B * exp(-k * age)), data = example_data, 
                   start = list(A = 600, B = 100, k = 0.01))

summary(brody_model)


brody_predicted <- data_validation[, c("RFID", "age", "smooth_dailywt")]
brody_predicted$predictions <- predict(brody_model, newdata = brody_predicted)


#RMSE_2
sqrt(mean((brody_predicted$predictions - brody_predicted$smooth_dailywt)^2, na.rm = TRUE))

# data_merged <- merge(data_model, data_predicted)

# rsq
model_rsq <- lm(smooth_dailywt ~ predictions, data = brody_predicted)
rsq <- summary(model_rsq)$r.squared
rsq

#mae
mean(abs(brody_predicted$smooth_dailywt - brody_predicted$predictions), na.rm = TRUE)

#mape
mean(abs((brody_predicted$smooth_dailywt - brody_predicted$predictions) / brody_predicted$smooth_dailywt), na.rm = TRUE) * 100


ggplot(brody_predicted, aes(x = age)) +
  geom_point(aes(y = predictions, color = "brody")) +
  geom_point(aes(y = smooth_dailywt))







ggplot(data_model, aes(x = age, y = predicted, color = RFID)) +
  geom_point() +
  guides(color = "none")

ggplot(example_data, aes(x = age, y = predicted)) +
  geom_line()



model_2 <- lmer(smooth_dailywt ~ age + sex + breed + ref_weight + monthly_rain + (1 | RFID), data = brody_model_data)

summary(model_2)

fixed_effects_2 <- fixef(model_2)
random_effects_2 <- ranef(model_2)

b0 <- fixed_effects_2["(Intercept)"]
b1 <- fixed_effects_2["age"]
b2 <- fixed_effects_2["sex1"]
b3 <- fixed_effects_2["breed2"]
b4 <- fixed_effects_2["breed3"]
b5 <- fixed_effects_2["breed4"]
b6 <- fixed_effects_2["ref_weight"]
b7 <- fixed_effects_2["monthly_rain"]


# vlidation 

data_predicted_2 <- brody_validation_data %>%
  group_by(RFID) %>%
  mutate(
    predicted_dailywt = b0 + b2 * as.numeric(sex == 1) +
      b3 * as.numeric(breed == 2) + b4 * as.numeric(breed == 3) + b5 * as.numeric(breed == 4) +
      b6 * ref_weight + b7 * monthly_rain +
      rnorm(n(), mean = 0, sd = summary(model_2)$sigma)
  ) %>%
  ungroup()

data_predicted_2$predicted_dailywt <- round(data_predicted_2$predicted_dailywt, 2)

# data_predicted_2 <- subset(data_predicted_2, !is.na(est_dailywt) & !is.nan(est_dailywt))

data_predicted_2$actual_diff <- data_predicted_2$predicted_dailywt - data_predicted_2$smooth_dailywt

squared_diff_2 <- (data_predicted_2$predicted_dailywt - data_predicted_2$smooth_dailywt)^2
RMSE_2 <- sqrt(mean(squared_diff_2, na.rm = TRUE))
RMSE_2

model_rsq_2 <- lm(smooth_dailywt ~ predicted_dailywt, data = data_predicted_2)
rsq_2 <- summary(model_rsq_2)$r.squared
rsq_2







#####################




# data_Brahman <- subset(data_model, breed == 1 & !is.na(smooth_dailywt))
# data_composite <- subset(data_model, breed == 2 & !is.na(smooth_dailywt))
# data_BrahxComp <- subset(data_model, breed == 3 & !is.na(smooth_dailywt))
# data_BelmontRed <- subset(data_model, breed == 4 & !is.na(smooth_dailywt))
# 
# data_Brahman_avg <- data_Brahman %>%
#   group_by(age) %>%
#   summarise(weight = round(mean(smooth_dailywt), 2))
# 
# data_Composite_avg <- data_composite %>%
#   group_by(age) %>%
#   summarise(weight = round(mean(smooth_dailywt), 2))
# 
# data_BrahxComp_avg <- data_BrahxComp %>%
#   group_by(age) %>%
#   summarise(weight = round(mean(smooth_dailywt), 2))
# 
# data_BelmontRed_avg <- data_BelmontRed %>%
#   group_by(age) %>%
#   summarise(weight = round(mean(smooth_dailywt), 2))
# 
# 
# brody_Brahman <- nls(weight ~ A * (1 - B * exp(-k * age)), data = data_Brahman_avg, 
#                    start = list(A = 600, B = 0.8, k = 0.0008))

# ggplot(data_Brahman_avg, aes(x = age, y = weight)) +
#   geom_point()
# 
# ggplot(data_Composite_avg, aes(x = age, y = weight)) +
#   geom_point()
# 
# ggplot(data_BrahxComp_avg, aes(x = age, y = weight)) +
#   geom_point()
# 
# ggplot(data_BelmontRed_avg, aes(x = age, y = weight)) +
#   geom_point()















# plotting the predicted vs actual wow
ggplot(data_predicted, aes(x = est_dailywt, y = predicted_dailywt)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue", formula = y ~ x) +
  labs(x = "Actual WO weights (kg)", y = "Predicted dailywts (kg)", title = "Predicted vs. Actual weights") +
  theme_minimal()

hist(data_predicted$actual_diff, main = "Histogram of Actual Differences", xlab = "Actual Difference")


# mean weight of animals for each day 
mean_actual <- data_predicted %>%
  group_by(age) %>%
  summarise(mean_actual = round(mean(est_dailywt, na.rm = TRUE), 2))

mean_predicted <- data_predicted %>%
  group_by(age) %>%
  summarise(mean_predicted = round(mean(predicted_dailywt, na.rm = TRUE), 2))

mean_dailywts <- merge(mean_actual, mean_predicted, by = "age")

# mean_staticwts <- staticwts_validation %>%
#   group_by(Date) %>%
#   summarise(mean_staticwts = round(mean(staticwt, na.rm = TRUE), 2))

ggplot(mean_dailywts, aes(x = age)) +
  geom_point(aes(y = mean_actual)) +
  geom_point(aes(y = mean_predicted, color = "mean_predicted"))
  



example_data <- subset(data_predicted, RFID == "951 000305782585")

ggplot(example_data, aes(x = age)) +
  geom_point(aes(y = predicted_dailywt)) +
  geom_point(aes(y = smooth_dailywt, color = "wow"))

ggplot(example_data, aes(x = age, y = predicted_dailywt)) +
  geom_point() +
  geom_smooth()


# predicted 200 day dailywt
predicted_200d <- subset(data_predicted, age == 200, select = c("RFID", "smooth_dailywt", "predicted_dailywt"))
colnames(predicted_200d)[ncol(predicted_200d)] <- "predicted_200d"




ggplot(predicted_200d, aes(x = smooth_dailywt, y = predicted_200d)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks = seq(0, 800, 100))
















# # # defining Brody growth curve model
# model_brody <- nls(mean_stwt ~ A * (1 - B * exp(-k * age)), data = brody_data, start = list(A = max(brody_data$mean_stwt), B = min(brody_data$mean_stwt), k = 0.01))
# 
# summary(model_brody)
# 
# # A, B and k values obtained from model fit
# A <- rep(7.683e+02, 156679)
# B <- rep(8.996e-01, 156679)
# k <- rep(8.511e-04, 156679)


# linear interpolation to get weight for missing days
# dailywts_splited <- split(dailywts_14d_avg, f = dailywts_14d_avg$RFID)
# 
# est_dailywts_list <- list()
# 
# for (i in seq_along(dailywts_splited)) {
#   est_dailywts_list[[i]] <- with(dailywts_splited[[i]], approx(x = Date, y = avg_14DW, xout = dates_dailywt)$y)
# }
# 
# dailywts_smooth <- cbind(dailywts_14d_avg, smooth_dailywt = unlist(est_dailywts_list))
# dailywts_smooth$smooth_dailywt <- round(dailywts_smooth$smooth_dailywt, 2)



####### All properties data analysis #####

all_dailywts <- get_dailywts()
all_DOB <- get_cattle(fields = c("RFID", "stationname", 
                                 "properties.sex", "properties.birthDate", 
                                 "properties.birthWeight", "properties.breed"))


# changing some column names and removing invalid records

all_dailywts <- select(all_dailywts, -X)
colnames(all_dailywts)[colnames(all_dailywts) == "Date"] <- "DateTime"
all_dailywts$Date <- as.Date(all_dailywts$DateTime, format = "%d-%m-%y")
colnames(all_dailywts)[colnames(all_dailywts) == "Weight"] <- "dailywt"
all_dailywts <- all_dailywts[, -c(3:5)]


all_DOB <- select(all_DOB, -c(X, X_id))
all_DOB <- subset(all_DOB, breed != "xxxxxx" & birthDate != "1970-01-01" & birthDate != "2023-01-01")
all_DOB$breed[all_DOB$breed == "Composite "] <- "Composite"

all <- merge(all_dailywts, all_DOB, by = "RFID")
all <- subset(all, birthWeight > 0)

unique(all$stationname)
unique(all_dailywts$Location)
length(unique(all$RFID))







































