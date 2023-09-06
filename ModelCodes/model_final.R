library(DMMongoDB)
library(tidyverse)
library(lme4)
library(lubridate)
library(gridExtra)
library(zoo)
library(DescTools)
library(randomForest)
library(rpart)


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


# outlier removal of dailywts
dailywts <- subset(dailywts_raw, dailywt >= min(DOB_data$birthWeight) & dailywt <= 2 * max(staticwts_out$staticwt))

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

# # selecting animals with at least 2 days of dailywts record
# dailywts_data <- dailywts_data %>%
#   group_by(RFID) %>%
#   filter(n_distinct(Date) >= 2) %>%
#   ungroup()


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


# merging dailywts and DOB, staticwts and weather data
dailywts_DOB_data <- merge(dailywts_rollavg, DOB_data, by = "RFID")

dailywts_staticwts_data <- merge(dailywts_DOB_data, staticwts_data, by = c("RFID", "Date"), all.x = TRUE, all.y = TRUE)

complete_data <- merge(dailywts_staticwts_data, weather_data_monthly, by = "Date", all.x = TRUE)

complete_data$age <- as.numeric(as.Date(complete_data$Date) - as.Date(complete_data$birthDate) + 1)

complete_data <- subset(complete_data, age >= 1 & age <= 900)

complete_data <- complete_data %>%
  mutate(smooth_dailywt = ifelse(age == 1, birthWeight, smooth_dailywt))

complete_data <- complete_data %>%
  mutate(staticwt = ifelse(age == 1, birthWeight, staticwt))

complete_data$breed[complete_data$breed == "Brahman"] <- 1
complete_data$breed[complete_data$breed == "Composite"] <- 2
complete_data$breed[complete_data$breed == "BrahxComp"] <- 3
complete_data$breed[complete_data$breed == "Belmont Red"] <- 4

complete_data$sex[complete_data$sex == "female"] <- 0
complete_data$sex[complete_data$sex == "male"] <- 1


# counting number of dailywt records for each animal

complete_dailywts <- subset(complete_data, !is.na(smooth_dailywt))

dailywts_count <- data.frame(RFID = character(), count = numeric())

for (i in unique(complete_dailywts$RFID)) {
  count <- data.frame(RFID = i, count = sum(complete_dailywts$RFID == i))
  dailywts_count <- rbind(dailywts_count, count)
}

dailywts_count <- dailywts_count %>%
  filter(count >= 2)


## 
# complete_dailywts <- complete_data %>%
#   filter(!is.na(smooth_dailywt)) %>%
#   mutate(AgeGroup = cut(age, breaks = c(0, 200, 400, 700), labels = c("0-200", "201-400", "401-700")))
# 
# selected_dailywts <- complete_dailywts %>%
#   group_by(RFID) %>%
#   summarise(NumDistinctAgeGroups = n_distinct(AgeGroup)) %>%
#   filter(NumDistinctAgeGroups == 3)


#splitting dataset for training (75%) and testing (25%) the models
set.seed(123)

proportion_model <- 0.75

train_animals <- sample(selected_RFID, size = floor(proportion_model * length(selected_RFID)))

test_animals <- setdiff(selected_RFID, train_animals)

data_train <- complete_data %>%
  filter(RFID %in% train_animals)

data_test <- complete_data %>%
  filter(RFID %in% test_animals)



# Model 1 ####
# Multiple linear regression (MLR)

model_mlr <- lm(smooth_dailywt ~ age + sex + breed + birthWeight + monthly_rain + monthly_temp, data = data_train)

summary(model_mlr)
AIC(model_mlr)
BIC(model_mlr)


data_predicted <- data_test[, c("RFID", "smooth_dailywt", "age", "sex", "breed", "birthWeight", "monthly_rain", "monthly_temp")]

data_predicted$predicted_mlr <- predict(model_mlr, newdata = data_predicted)

data_predicted$predicted_mlr <- round(data_predicted$predicted_mlr, 2)


#RMSE_mlr
sqrt(mean((data_predicted$predicted_mlr - data_predicted$smooth_dailywt)^2, na.rm = TRUE))

#CCC_mlr
CCC(data_predicted$smooth_dailywt, data_predicted$predicted_mlr, ci = "z-transform", conf.level = 0.95, na.rm = TRUE)$rho.c

#rsq_mlr
summary(lm(smooth_dailywt ~ predicted_mlr, data = data_predicted))$r.squared


#mae_mlr
mean(abs(data_predicted$smooth_dailywt - data_predicted$predicted_mlr), na.rm = TRUE)

#mape_mlr
mean(abs((data_predicted$smooth_dailywt - data_predicted$predicted_mlr) / data_predicted$smooth_dailywt), na.rm = TRUE) * 100




# model 2 ####
# Random forest (RF)

data_rf <- na.omit(data_train[c("age", "sex", "breed", "birthWeight", "smooth_dailywt", "monthly_rain", "monthly_temp")])

features <- c("age", "sex", "breed", "birthWeight", "monthly_rain", "monthly_temp")
target <- "smooth_dailywt"
train_data <- data_rf[features]

# Fit the Random Forest model
model_rf <- randomForest(train_data, data_rf[[target]])

summary(model_rf)

data_predict <- na.omit(data_test[, c("RFID", "age", "sex", "breed", "birthWeight", "monthly_rain", "monthly_temp")])

data_predicted$predicted_rf <- predict(model_rf, newdata = data_predict)

data_predicted$predicted_rf <- round(data_predicted$predicted_rf, 2)

#RMSE_rf
sqrt(mean((data_predicted$predicted_rf - data_predicted$smooth_dailywt)^2, na.rm = TRUE))


# rsq_rf
summary(lm(smooth_dailywt ~ predicted_rf, data = data_predicted))$r.squared


#mae_rf
mean(abs(data_predicted$smooth_dailywt - data_predicted$predicted_rf), na.rm = TRUE)

#mape_rf
mean(abs((data_predicted$smooth_dailywt - data_predicted$predicted_rf) / data_predicted$smooth_dailywt), na.rm = TRUE) * 100



# # decision tree model
# 
# model_dt <- rpart(smooth_dailywt ~ age + sex + breed + birthWeight + monthly_rain + monthly_temp, data = data_train)
# 
# summary(model_dt)
# # plot(model_dt)
# # text(model_dt)
# 
# # tree_predicted <- data_validation[, c("RFID", "smooth_dailywt", "age", "sex", "breed", "birthWeight", "monthly_rain", "monthly_temp")]
# 
# data_predicted$predicted_dt <- predict(model_dt, newdata = data_test)
# 
# 
# #RMSE_2
# sqrt(mean((data_predicted$predicted_dt - data_predicted$smooth_dailywt)^2, na.rm = TRUE))
# 
# # data_merged <- merge(data_model, data_predicted)
# 
# # rsq
# summary(lm(smooth_dailywt ~ predicted_dt, data = data_predicted))$r.squared
# 
# #mae
# mean(abs(data_predicted$smooth_dailywt - data_predicted$predicted_dt), na.rm = TRUE)
# 
# #mape
# mean(abs((data_predicted$smooth_dailywt - data_predicted$predicted_dt) / data_predicted$smooth_dailywt), na.rm = TRUE) * 100



# plotting ###

plot_model_mlr <- ggplot(data_predicted, aes(x = smooth_dailywt, y = predicted_mlr)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue", formula = y ~ x) +
  labs(x = "smoothed WO weights (kg)", y = "Predicted weights (kg)") +
  theme_minimal()

plot_model_rf <- ggplot(data_predicted, aes(x = smooth_dailywt, y = predicted_rf)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue", formula = y ~ x) +
  labs(x = "Smoothed WO weights (kg)", y = "Predicted weights (kg)") +
  theme_minimal()

grid.arrange(plot_model_mlr, plot_model_rf, nrow = 1)


data_avg <- data_predicted %>%
  mutate(age_interval = age) %>%
  group_by(age_interval) %>%
  summarise(dailywt_avg = round(mean(smooth_dailywt, na.rm = TRUE), 2),
            mlr_avg = round(mean(predicted_mlr, na.rm = TRUE), 2),
            rf_avg = round(mean(predicted_rf, na.rm = TRUE), 2))


data_dailywts <- subset(complete_data, !is.na(smooth_dailywt) & age !=1)

ggplot() +
  geom_histogram(data = data_dailywts, aes(age), binwidth = 10, color = "black", fill = "grey") +
  # geom_point(data = data_avg, aes(x = age_interval, y = dailywt_avg * 10)) +
  geom_line(data = data_avg[!is.nan(data_avg$dailywt_avg), ], aes(x = age_interval, y = dailywt_avg * 5)) +
  scale_x_continuous(name = "Age (days)", breaks = seq(0, 900, by = 100), limits = c(0, 900)) +
  scale_y_continuous(name = "No. of walk-over weight records", breaks = seq(0, 4000, by = 500), 
                     sec.axis = sec_axis(trans = ~ ./5, name = "Weight (kg)", breaks = seq(0, 600, by = 100)))



ggplot() +
  geom_line(data = data_avg[!is.nan(data_avg$dailywt_avg), ], 
            aes(x = age_interval, y = dailywt_avg, linetype = "Smoothed WO weight")) +
  geom_line(data = data_avg[!is.nan(data_avg$mlr_avg), ], 
            aes(x = age_interval, y = mlr_avg, linetype = "MLR prediction")) +
  geom_line(data = data_avg[!is.nan(data_avg$rf_avg), ], 
            aes(x = age_interval, y = rf_avg, linetype = "RF prediction")) +
  labs(linetype = NULL) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.75, 0.28)) +
  scale_x_continuous(name = "Age (days)", breaks = seq(0, 900, by = 100), limits = c(0, 900)) +
  scale_y_continuous(name = "Weight (kg)", breaks = seq(0, 600, by = 100), limits = c(0, 600)) +
  scale_linetype_manual(values = c("Smoothed WO weight" = "solid",
                                   "MLR prediction" = "dashed",
                                   "RF prediction" = "longdash"))


# tables























