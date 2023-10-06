library(DMMongoDB)
library(tidyverse)
library(lme4)
library(lubridate)
library(gridExtra)
library(zoo)
library(DescTools)
library(randomForest)
library(xgboost)
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
Belmont_DOB <- Belmont_DOB[-c(2333, 2354), ]


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
  mutate(monthly_temp = round(mean(avg_temp), 2))

weather_data_monthly <- weather_data[, -c(2:6)]


# selecting animals for which birthDate, birthWeight, staticwt and dailywt available

common_RFID <- intersect(intersect(unique(staticwts_alltime$RFID), unique(dailywts_alltime$RFID)), Belmont_DOB$RFID)

DOB_data <- Belmont_DOB %>%
  filter(RFID %in% common_RFID) %>%
  distinct()

dailywts_all <- dailywts_alltime %>%
  filter(RFID %in% common_RFID)

staticwts_all <- staticwts_alltime %>%
  filter(RFID %in% common_RFID)

staticwts_data <- subset(staticwts_all, staticwt >= 19)

# averaging staticwts if more than one record in a day
staticwts_data <- staticwts_data %>%
  group_by(RFID, Date) %>%
  summarise(staticwt = round(mean(staticwt), 2)) %>%
  ungroup()

dailywts_data <- subset(dailywts_all, dailywt >= 19 & dailywt <= max(staticwts_data$staticwt))


# approach to find optimum window size for rolling average for smoothing of wow ####

#selecting static weights between 2023-03-03 to 2023-03-31
staticwts_rollavg <- subset(staticwts_data, Date >= "2023-03-03" & Date <= "2023-03-31")

ggplot(staticwts_rollavg, aes(Date, y = staticwt)) +
  geom_point()


# # selecting wow between 2023-02-21 to 2023-04-10 (10 day prior and 10 days after of static weight days) for animals selected

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


allwts_rollavg <- merge(dailywts_roll, staticwts_rollavg, by = c("RFID", "Date"))


simulated_data <- allwts_rollavg[, !(names(allwts_rollavg) %in% c("RFID", "Date", "dailywt", "staticwt", "est_staticwts"))]

CCC_values <- data.frame(window_size = numeric(), ccc = numeric())

for (col_name in names(simulated_data)) {
  ccc_result <- CCC(simulated_data[[col_name]], allwts_rollavg$staticwt, ci = "z-transform", conf.level = 0.95, na.rm = TRUE)
  
  CCC_values <- rbind(CCC_values, data.frame(window = col_name, ccc = ccc_result$rho.c))
}

CCC_values <- data.frame(window_size = 1:20, ccc = round(CCC_values$ccc.est, 3))

CCC_values$ccc_diff <- c(0, diff(CCC_values$ccc))


ggplot(CCC_values, aes(x = window_size, y = ccc)) +
  geom_line() + geom_point() + xlab("Window size") + ylab("CCC") +
  scale_y_continuous(breaks = seq(0.4, 1, by = 0.1)) +
  scale_x_continuous(breaks = seq(0, 30, by = 4))





### Outlier deletion ####

# outlier removal of dailywts 

dates_dailywt <- seq(min(as.Date(dailywts_data$Date)), max(as.Date(dailywts_data$Date)), by = "day")
new_dailywts <- expand.grid(RFID = unique(dailywts_data$RFID), Date = dates_dailywt)

merged_dailywts <- merge(new_dailywts, dailywts_data, by = c("RFID", "Date"), all.x = TRUE)

dailywts_raw <- list()

for (i in unique(merged_dailywts$RFID)) {
  animal_data <- subset(merged_dailywts, RFID == i)
  
  roll_avg <- rollapply(animal_data$dailywt, width = 12, align = 'center', FUN = mean, fill = NA, na.rm = TRUE)
  
  roll_sd <- rollapply(animal_data$dailywt, width = 12, align = 'center', FUN = sd, fill = NA, na.rm = TRUE)
  
  rollavg_animal_data <- cbind(animal_data, roll_avg, roll_sd)
  
  dailywts_raw <- rbind(dailywts_raw, rollavg_animal_data)
}

dailywts_raw$roll_avg <- round(dailywts_raw$roll_avg, 2)
dailywts_raw$roll_sd <- round(dailywts_raw$roll_sd, 2)

dailywts_raw <- subset(dailywts_raw, !is.na(dailywts_raw$dailywt))


dailywts_raw$lower_limit <- dailywts_raw$roll_avg - dailywts_raw$roll_sd
dailywts_raw$upper_limit <- dailywts_raw$roll_avg + dailywts_raw$roll_sd

dailywts_out <- subset(dailywts_raw, dailywt >= lower_limit & dailywt <= upper_limit)


# averaging dailywts if more than one record in a day
dailywts_out <- dailywts_out %>%
  group_by(RFID, Date) %>%
  summarise(dailywt = round(mean(dailywt), 2)) %>%
  ungroup()


# using 12 days rolling average to smooth the dailywts_data (max CCC and least RMSE) ####

dates_dailywt_smooth <- seq(min(as.Date(dailywts_out$Date)), max(as.Date(dailywts_out$Date)), by = "day")
new_dailywts_smooth <- expand.grid(RFID = unique(dailywts_out$RFID), Date = dates_dailywt_smooth)

merged_dailywts_smooth <- merge(new_dailywts_smooth, dailywts_out, by = c("RFID", "Date"), all.x = TRUE)

dailywts_smooth <- list()

for (i in unique(merged_dailywts_smooth$RFID)) {
  animal_data <- subset(merged_dailywts_smooth, RFID == i)
  
  smooth_dailywt <- rollapply(animal_data$dailywt, width = 12, align = 'center', FUN = mean, fill = NA, na.rm = TRUE)
  
  rollavg_animal_data <- cbind(animal_data, smooth_dailywt)
  
  dailywts_smooth <- rbind(dailywts_smooth, rollavg_animal_data)
}

dailywts_smooth$smooth_dailywt <- round(dailywts_smooth$smooth_dailywt, 2)

dailywts_smooth$smooth_dailywt[is.nan(dailywts_smooth$smooth_dailywt)] <- NA



# merging dailywts and DOB, staticwts and weather data

complete_data <- merge(merge(dailywts_smooth, DOB_data, by = "RFID"), weather_data_monthly, by = "Date", all.x = TRUE)

complete_data$age <- as.numeric(as.Date(complete_data$Date) - as.Date(complete_data$birthDate) + 1)




ggplot(selected_data, aes(x = age, y = smooth_dailywt, color = RFID)) +
  geom_line() +
  guides(color = "none")



complete_data$breed[complete_data$breed == "Brahman"] <- 1
complete_data$breed[complete_data$breed == "Composite"] <- 2
complete_data$breed[complete_data$breed == "BrahxComp"] <- 3
complete_data$breed[complete_data$breed == "Belmont Red"] <- 4

complete_data$sex[complete_data$sex == "female"] <- 0
complete_data$sex[complete_data$sex == "male"] <- 1



complete_dailywts <- subset(complete_data, !is.na(smooth_dailywt))

dailywts_count <- data.frame(RFID = character(), count = numeric())

for (i in unique(complete_dailywts$RFID)) {
  count <- data.frame(RFID = i, count = sum(complete_dailywts$RFID == i))
  dailywts_count <- rbind(dailywts_count, count)
}

dailywts_count <- dailywts_count %>%
  filter(count >= 2)


selected_RFID <- dailywts_count$RFID

selected_data <- complete_data %>%
  filter(RFID %in% selected_RFID) %>%
  select(RFID, smooth_dailywt, age, sex, breed, birthWeight, monthly_rain, monthly_temp)

selected_data <- subset(complete_data, age >= 30 & age <= 900)


#splitting dataset for training (75%) and testing (25%) the models

set.seed(123)

proportion_model <- 0.7

train_animals <- sample(selected_RFID, size = floor(proportion_model * length(selected_RFID)))

test_animals <- setdiff(selected_RFID, train_animals)

data_train <- selected_data %>%
  filter(RFID %in% train_animals)

data_test <- selected_data %>%
  filter(RFID %in% test_animals)


# Model 1 ####
# Multiple linear regression (MLR)

model_mlr <- lm(smooth_dailywt ~ age + sex + breed + birthWeight + monthly_rain + monthly_temp, data = data_train)

summary(model_mlr)

# data_predicted <- data_test[, c("RFID", "smooth_dailywt", "age", "sex", "breed", "birthWeight", "monthly_rain", "monthly_temp")]

data_test$predicted_mlr <- predict(model_mlr, newdata = data_test)

data_test$predicted_mlr <- round(data_test$predicted_mlr, 2)


#RMSE_mlr
sqrt(mean((data_test$predicted_mlr - data_test$smooth_dailywt)^2, na.rm = TRUE))

#mae_mlr
mean(abs(data_test$smooth_dailywt - data_test$predicted_mlr), na.rm = TRUE)

#mape_mlr
mean(abs((data_test$smooth_dailywt - data_test$predicted_mlr) / data_test$smooth_dailywt), na.rm = TRUE) * 100

#CCC_mlr
CCC(data_test$smooth_dailywt, data_test$predicted_mlr, ci = "z-transform", conf.level = 0.95, na.rm = TRUE)$rho.c

#rsq_mlr
summary(lm(smooth_dailywt ~ predicted_mlr, data = data_test))$r.squared


# model 2 ####
# Random forest (RF)

data_rf <- na.omit(data_train[c("age", "sex", "breed", "birthWeight", "smooth_dailywt", "monthly_rain", "monthly_temp")])

features <- c("age", "sex", "breed", "birthWeight", "monthly_rain", "monthly_temp")
target <- "smooth_dailywt"
train_data <- data_rf[features]

# Fit the Random Forest model
model_rf <- randomForest(train_data, data_rf[[target]], ntree = 500, mtry = 2)

summary(model_rf)

data_predict <- na.omit(data_test[, c("RFID", "age", "sex", "breed", "birthWeight", "monthly_rain", "monthly_temp")])

data_test$predicted_rf <- predict(model_rf, newdata = data_predict)

data_test$predicted_rf <- round(data_test$predicted_rf, 2)

#RMSE_rf
sqrt(mean((data_test$predicted_rf - data_test$smooth_dailywt)^2, na.rm = TRUE))

#mae_rf
mean(abs(data_test$smooth_dailywt - data_test$predicted_rf), na.rm = TRUE)

#mape_rf
mean(abs((data_test$smooth_dailywt - data_test$predicted_rf) / data_test$smooth_dailywt), na.rm = TRUE) * 100

#CCC_rf
CCC(data_test$smooth_dailywt, data_test$predicted_rf, ci = "z-transform", conf.level = 0.95, na.rm = TRUE)$rho.c

# rsq_rf
summary(lm(smooth_dailywt ~ predicted_rf, data = data_test))$r.squared



# Extreme Gradient Boosting (XGBoost) ####
train_xgb <- na.omit(data_train)

model_xgb <- xgboost(
  data = data.matrix(train_xgb[, c("age", "sex", "breed", "birthWeight", "monthly_rain", "monthly_temp")]),
  label = train_xgb$smooth_dailywt,
  nrounds = 100,  # Number of boosting rounds (adjust as needed)
  objective = "reg:squarederror",  # Use squared error as the objective for regression
  max_depth = 5,  # Maximum tree depth (adjust as needed)
  eta = 0.1  # Learning rate (adjust as needed)
)

summary(model_xgb)

data_test$predicted_xgb <- predict(model_xgb, newdata = data.matrix(data_test[, c("age", "sex", "breed", "birthWeight", "birthWeight", "monthly_rain", "monthly_temp")]))
data_test$predicted_xgb <- round(data_test$predicted_xgb, 2)


#RMSE_xgb
sqrt(mean((data_test$predicted_xgb - data_test$smooth_dailywt)^2, na.rm = TRUE))

#mae_xgb
mean(abs(data_test$smooth_dailywt - data_test$predicted_xgb), na.rm = TRUE)

#mape_xgb
mean(abs((data_test$smooth_dailywt - data_test$predicted_xgb) / data_test$smooth_dailywt), na.rm = TRUE) * 100

# rsq_xgb
summary(lm(smooth_dailywt ~ predicted_xgb, data = data_test))$r.squared




# model predictions

data_predicted <- selected_data

data_predicted$predicted_mlr <- predict(model_mlr, newdata = data_predicted)
data_predicted$predicted_mlr <- round(data_predicted$predicted_mlr, 2)


predict_rf <- na.omit(data_predicted[, c("RFID", "age", "sex", "breed", "birthWeight", "monthly_rain", "monthly_temp")])

data_predicted$predicted_rf <- predict(model_rf, newdata = predict_rf)
data_predicted$predicted_rf <- round(data_predicted$predicted_rf, 2)


data_predicted$predicted_xgb <- predict(model_xgb, newdata = data.matrix(data_predicted[, c("age", "sex", "breed", "birthWeight", "monthly_rain", "monthly_temp")]))
data_predicted$predicted_xgb <- round(data_predicted$predicted_xgb, 2)


# plotting ####

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

plot_model_xgb <- ggplot(data_predicted, aes(x = smooth_dailywt, y = predicted_xgb)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue", formula = y ~ x) +
  labs(x = "Smoothed WO weights (kg)", y = "Predicted weights (kg)") +
  theme_minimal()

grid.arrange(plot_model_mlr, plot_model_rf, plot_model_xgb, nrow = 1)


data_avg <- data_predicted %>%
  group_by(age) %>%
  summarise(dailywt_avg = round(mean(smooth_dailywt, na.rm = TRUE), 2),
            mlr_avg = round(mean(predicted_mlr, na.rm = TRUE), 2),
            rf_avg = round(mean(predicted_rf, na.rm = TRUE), 2),
            xgb_avg = round(mean(predicted_rf, na.rm = TRUE), 2))


data_dailywts <- subset(data_predicted, !is.na(smooth_dailywt))

ggplot() +
  geom_histogram(data = data_dailywts, aes(age), binwidth = 10, color = "black", fill = "grey") +
  # geom_point(data = data_avg, aes(x = age, y = dailywt_avg * 5)) +
  geom_line(data = data_avg[!is.nan(data_avg$dailywt_avg), ], aes(x = age, y = dailywt_avg * 5)) +
  scale_x_continuous(name = "Age (days)", breaks = seq(0, 900, by = 100), limits = c(0, 900)) +
  scale_y_continuous(name = "No. of WOW records", breaks = seq(0, 4000, by = 500), 
                     sec.axis = sec_axis(trans = ~ ./5, name = "Weight (kg)", breaks = seq(0, 600, by = 100)))



ggplot() +
  geom_line(data = data_avg[!is.nan(data_avg$dailywt_avg), ], 
            aes(x = age, y = dailywt_avg, linetype = "Smoothed WOW")) +
  geom_line(data = data_avg[!is.nan(data_avg$mlr_avg), ], 
            aes(x = age, y = mlr_avg, linetype = "MLR prediction")) +
  # geom_line(data = data_avg[!is.nan(data_avg$rf_avg), ], 
  #           aes(x = age, y = rf_avg, linetype = "RF prediction")) +
  geom_line(data = data_avg[!is.nan(data_avg$xgb_avg), ], 
            aes(x = age, y = rf_avg, linetype = "XGB prediction")) +
  labs(linetype = NULL) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.75, 0.28)) +
  scale_x_continuous(name = "Age (days)", breaks = seq(0, 900, by = 100), limits = c(0, 900)) +
  scale_y_continuous(name = "Weight (kg)", breaks = seq(0, 600, by = 100), limits = c(0, 600)) +
  scale_linetype_manual(values = c("Smoothed WOW" = "solid",
                                   "MLR prediction" = "dotted",
                                   # "RF prediction" = "longdash",
                                   "XGB prediction" = "longdash"))




# tables ####
# getting 200-day, 400-day and 600-day weights

data_predicted <- data_predicted %>%
  group_by(RFID) %>%
  mutate(est_dailywt = approx(x = age, y = smooth_dailywt, xout = age)$y)

data_predicted$est_dailywt <- round(data_predicted$est_dailywt, 2)

wow_200d <- subset(data_predicted, age == 200 & !is.na(est_dailywt))

wow_400d <- subset(data_predicted, age == 400 & !is.na(est_dailywt))

wow_600d <- subset(data_predicted, age == 600 & !is.na(est_dailywt))

example_data <- subset(wow_200d, sex == 0 & breed == 1)
# #RMSE_mlr
# sqrt(mean((wow_200d$predicted_mlr - wow_200d$est_dailywt)^2, na.rm = TRUE))
# sqrt(mean((wow_400d$predicted_mlr - wow_400d$est_dailywt)^2, na.rm = TRUE))
# sqrt(mean((wow_600d$predicted_mlr - wow_600d$est_dailywt)^2, na.rm = TRUE))
# 
# #mae_mlr
# mean(abs(wow_200d$est_dailywt - wow_200d$predicted_mlr), na.rm = TRUE)
# mean(abs(wow_400d$est_dailywt - wow_400d$predicted_mlr), na.rm = TRUE)
# mean(abs(wow_600d$est_dailywt - wow_600d$predicted_mlr), na.rm = TRUE)
# 
# #mape_mlr
# mean(abs((wow_200d$est_dailywt - wow_200d$predicted_mlr) / wow_200d$est_dailywt), na.rm = TRUE) * 100
# mean(abs((wow_400d$est_dailywt - wow_400d$predicted_mlr) / wow_400d$est_dailywt), na.rm = TRUE) * 100
# mean(abs((wow_600d$est_dailywt - wow_600d$predicted_mlr) / wow_600d$est_dailywt), na.rm = TRUE) * 100
# 
# # rsq_mlr
# summary(lm(est_dailywt ~ predicted_mlr, data = wow_200d))$r.squared
# summary(lm(est_dailywt ~ predicted_mlr, data = wow_400d))$r.squared
# summary(lm(est_dailywt ~ predicted_mlr, data = wow_600d))$r.squared
# 
# 
# #RMSE_rf
# sqrt(mean((wow_200d$predicted_rf - wow_200d$est_dailywt)^2, na.rm = TRUE))
# sqrt(mean((wow_400d$predicted_rf - wow_400d$est_dailywt)^2, na.rm = TRUE))
# sqrt(mean((wow_600d$predicted_rf - wow_600d$est_dailywt)^2, na.rm = TRUE))
# 
# #mae_rf
# mean(abs(wow_200d$est_dailywt - wow_200d$predicted_rf), na.rm = TRUE)
# mean(abs(wow_400d$est_dailywt - wow_400d$predicted_rf), na.rm = TRUE)
# mean(abs(wow_600d$est_dailywt - wow_600d$predicted_rf), na.rm = TRUE)
# 
# #mape_rf
# mean(abs((wow_200d$est_dailywt - wow_200d$predicted_rf) / wow_200d$est_dailywt), na.rm = TRUE) * 100
# mean(abs((wow_400d$est_dailywt - wow_400d$predicted_rf) / wow_400d$est_dailywt), na.rm = TRUE) * 100
# mean(abs((wow_600d$est_dailywt - wow_600d$predicted_rf) / wow_600d$est_dailywt), na.rm = TRUE) * 100
# 
# # rsq_rf
# summary(lm(est_dailywt ~ predicted_rf, data = wow_200d))$r.squared
# summary(lm(est_dailywt ~ predicted_rf, data = wow_400d))$r.squared
# summary(lm(est_dailywt ~ predicted_rf, data = wow_600d))$r.squared

# descriptive statistics

#200d
min(wow_200d$est_dailywt)
mean(wow_200d$est_dailywt)
max(wow_200d$est_dailywt)
sd(wow_200d$est_dailywt)
sd(wow_200d$est_dailywt)/mean(wow_200d$est_dailywt) * 100

min(wow_200d$predicted_mlr)
mean(wow_200d$predicted_mlr)
max(wow_200d$predicted_mlr)
sd(wow_200d$predicted_mlr)
sd(wow_200d$predicted_mlr)/mean(wow_200d$predicted_mlr) * 100

min(wow_200d$predicted_rf)
mean(wow_200d$predicted_rf)
max(wow_200d$predicted_rf)
sd(wow_200d$predicted_rf)
sd(wow_200d$predicted_rf)/mean(wow_200d$predicted_rf) * 100

#400d
min(wow_400d$est_dailywt)
mean(wow_400d$est_dailywt)
max(wow_400d$est_dailywt)
sd(wow_400d$est_dailywt)
sd(wow_400d$est_dailywt)/mean(wow_400d$est_dailywt) * 100

min(wow_400d$predicted_mlr)
mean(wow_400d$predicted_mlr)
max(wow_400d$predicted_mlr)
sd(wow_400d$predicted_mlr)
sd(wow_400d$predicted_mlr)/mean(wow_400d$predicted_mlr) * 100

min(wow_400d$predicted_rf)
mean(wow_400d$predicted_rf)
max(wow_400d$predicted_rf)
sd(wow_400d$predicted_rf)
sd(wow_400d$predicted_rf)/mean(wow_400d$predicted_rf) * 100


#600d
min(wow_600d$est_dailywt)
mean(wow_600d$est_dailywt)
max(wow_600d$est_dailywt)
sd(wow_600d$est_dailywt)
sd(wow_600d$est_dailywt)/mean(wow_600d$est_dailywt) * 100

min(wow_600d$predicted_mlr)
mean(wow_600d$predicted_mlr)
max(wow_600d$predicted_mlr)
sd(wow_600d$predicted_mlr)
sd(wow_600d$predicted_mlr)/mean(wow_600d$predicted_mlr) * 100

min(wow_600d$predicted_rf)
mean(wow_600d$predicted_rf)
max(wow_600d$predicted_rf)
sd(wow_600d$predicted_rf)
sd(wow_600d$predicted_rf)/mean(wow_600d$predicted_rf) * 100


# other calculations for manuscript #####
####
min(selected_data$smooth_dailywt, na.rm = TRUE)
max(selected_data$smooth_dailywt, na.rm = TRUE)
mean(selected_data$smooth_dailywt, na.rm = TRUE)
sd(selected_data$smooth_dailywt, na.rm = TRUE)

selected_data_na <- subset(selected_data, !is.na(smooth_dailywt))




####
weights_before600 <- subset(selected_data, age <= 600 & !is.na(smooth_dailywt))
weights_after600 <- subset(selected_data, age > 600 & !is.na(smooth_dailywt))


before600_count <- data.frame(RFID = character(), count = numeric())

for (i in unique(weights_before600$RFID)) {
  count <- data.frame(RFID = i, count = sum(weights_before600$RFID == i))
  before600_count <- rbind(before600_count, count)
}

after600_count <- data.frame(RFID = character(), count = numeric())

for (i in unique(weights_before600$RFID)) {
  count <- data.frame(RFID = i, count = sum(weights_after600$RFID == i))
  after600_count <- rbind(after600_count, count)
}

t.test(before600_count$count, after600_count$count)$p.value


####
min(data_predicted$predicted_mlr)
max(data_predicted$predicted_mlr)
mean(data_predicted$predicted_mlr)
sd(data_predicted$predicted_mlr)

min(data_predicted$predicted_rf)
max(data_predicted$predicted_rf)
mean(data_predicted$predicted_rf)
sd(data_predicted$predicted_rf)





















