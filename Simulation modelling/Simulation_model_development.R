library(DMMongoDB)
library(tidyverse)
library(lubridate)
library(gridExtra)
library(xgboost)

#Data pre-processing and cleaning ####
Belmont_stwts <- read.csv(file = "Belmont_staticwts_alltime.csv")
Belmont_DOB <- read.csv(file = "Belmont_DOB.csv")


# changing some column names and removing invalid records
Belmont_stwts <- select(Belmont_stwts, -X)
Belmont_stwts$Date <- as.Date(Belmont_stwts$Date)
Belmont_stwts <- Belmont_stwts[Belmont_stwts$Date != "0014-06-20", ]
Belmont_stwts <- Belmont_stwts[, -4]


Belmont_DOB <- select(Belmont_DOB, -c(X, X_id))
Belmont_DOB <- subset(Belmont_DOB, birthWeight !=0 & birthDate != "1970-01-01" & birthDate != "2023-01-01")
Belmont_DOB$breed[Belmont_DOB$breed == "Composite "] <- "Composite"

all_data <- merge(Belmont_stwts, Belmont_DOB, by = "RFID")


# outlier removal of weight records and other processing ####

data_raw <- subset(all_data, Weight >= 19)

data_raw <- data_raw %>%
  group_by(RFID) %>%
  mutate(mean_wt = round(mean(Weight), 2), sd_wt = round(sd(Weight), 2))


data_out <- data.frame()

for (i in unique(data_raw$RFID)) {
  animal_data <- subset(data_raw, RFID == i)
  
  lower_limit <- animal_data$mean_wt - 1.5 * animal_data$sd_wt
  upper_limit <- animal_data$mean_wt + 1.5 * animal_data$sd_wt
  
  filtered_animal_data <- subset(animal_data, Weight >= lower_limit & Weight <= upper_limit)
  data_out <- rbind(data_out, filtered_animal_data)
}


# averaging if more than 1 record in a single day
data_clean <- data_out %>%
  group_by(RFID, Date) %>%
  summarise(Weight = round(mean(Weight), 2)) %>%
  ungroup()

data_clean <- data_clean %>%
  group_by(RFID) %>%
  filter(n_distinct(Weight) >=2)


#combine data
data_combined <- merge(data_clean, Belmont_DOB, by = "RFID")
data_combined <- data_combined[!duplicated(data_combined), ]

data_combined$age <- as.numeric(as.Date(data_combined$Date) - as.Date(data_combined$birthDate) + 1)

new_data <- data_combined %>%
  group_by(RFID) %>%
  reframe(age = 1:max(age))

complete_data <- merge(new_data, data_combined, by = c("RFID", "age"), all.x = TRUE)


complete_data <- complete_data %>%
  group_by(RFID) %>%
  fill(sex, breed, birthDate, birthWeight, category, .direction = "downup") %>%
  ungroup()

complete_data$Date <- as.Date(complete_data$birthDate) + complete_data$age - 1

complete_data <- complete_data %>%
  mutate(Weight = if_else(age == 1, birthWeight, Weight))


# predicting missing weights using ML (XGB) model ####
#splitting dataset for training (70%) and testing (30%) the models

set.seed(1)

selected_RFID <- as.character(unique(complete_data$RFID))

proportion_model <- 0.7

train_animals <- sample(selected_RFID, size = floor(proportion_model * length(selected_RFID)))

test_animals <- setdiff(selected_RFID, train_animals)

data_train <- complete_data %>%
  filter(RFID %in% train_animals)

data_test <- complete_data %>%
  filter(RFID %in% test_animals)



# Extreme Gradient Boosting (XGBoost) ####
train_xgb <- na.omit(data_train)

# hyper-parameter tuning
param_grid <- expand.grid(
  max_depth = c(1, 3, 5),
  learning_rate = c(0.1, 0.5, 1),
  nthread = c(1, 2, 3),
  nrounds = c(100, 200, 300)
)


best_hyperparameters <- NULL
best_metric <- Inf


for (h in seq(nrow(param_grid))) {
  hyperparams <- param_grid[h, ]
  xgb_model <- xgboost(
    data = data.matrix(train_xgb[, c("age", "sex", "breed", "birthWeight", "category")]),
    label = train_xgb$Weight,
    max_depth = hyperparams$max_depth,
    learning_rate = hyperparams$learning_rate,
    nthread = hyperparams$nthread,
    nrounds = hyperparams$nrounds,
    objective = "reg:squarederror",
    verbose = 0
  )
  y_pred <- predict(xgb_model, newdata = data.matrix(data_test[, c("age", "sex", "breed", "birthWeight", "category")]))
  rmse <- sqrt(mean((y_pred - data_test$Weight)^2, na.rm = TRUE))
  
  if (rmse < best_metric) {
    best_metric <- rmse
    best_hyperparameters <- hyperparams
  }
}


# model fit
model_xgb <- xgboost(
  data = data.matrix(train_xgb[, c("age", "sex", "breed", "birthWeight", "category")]),
  label = train_xgb$Weight,
  max_depth = best_hyperparameters$max_depth,
  learning_rate = best_hyperparameters$learning_rate,
  nthread = best_hyperparameters$nthread,
  nrounds = best_hyperparameters$nrounds,
  objective = "reg:squarederror",
  verbose = 0
)


data_test$predicted_xgb <- predict(model_xgb, newdata = data.matrix(data_test[, c("age", "sex", "breed", "birthWeight", "category")]))
data_test$predicted_xgb <- round(data_test$predicted_xgb, 2)


#RMSE_xgb
sqrt(mean((data_test$predicted_xgb - data_test$Weight)^2, na.rm = TRUE))

#mae_xgb
mean(abs(data_test$Weight - data_test$predicted_xgb), na.rm = TRUE)

#mape_xgb
mean(abs((data_test$Weight - data_test$predicted_xgb) / data_test$Weight), na.rm = TRUE) * 100

# rsq_xgb
summary(lm(Weight ~ predicted_xgb, data = data_test))$r.squared



# model predictions on complete_data 

data_predicted <- complete_data

data_predicted$predicted <- predict(model_xgb, newdata = data.matrix(data_predicted[, c("age", "sex", "breed", "birthWeight", "category")]))
data_predicted$predicted <- round(data_predicted$predicted, 2)


# plotting ML prediction results ####

scatter_xgb <- ggplot(data_predicted, aes(x = Weight, y = predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue", formula = y ~ x) +
  labs(x = "Actual weights (kg)", y = "Predicted weights (kg)") +
  ggtitle("(e)") + theme(plot.title = element_text(hjust = 0.5))

scatter_xgb


######################################################################
# stochastic simulation model ####
# finding daily growth rate
data_predicted <- data_predicted %>%
  arrange(RFID, Date) %>%
  group_by(RFID) %>%
  mutate(daily_growth = c(NA, diff(predicted)))


data_predicted <- mutate(data_predicted, season = case_when(
  as.numeric(format(Date, "%m")) %in% c(12, 01, 02) ~ "Summer",
  as.numeric(format(Date, "%m")) %in% c(03, 04, 05)   ~ "Autumn",
  as.numeric(format(Date, "%m")) %in% c(06, 07, 08)   ~ "Winter",
  as.numeric(format(Date, "%m")) %in% c(09, 10, 11) ~ "Spring"
))


growth_rates <- data_predicted %>%
  group_by(season, sex) %>%
  summarise(avg_growth = round(mean(daily_growth, na.rm = TRUE), 2),
         sd_growth = round(sd(daily_growth, na.rm = TRUE), 2))

growth_rates



# simulation model function

sim_model <- function(n_males, n_females, n_days, min_birthDate, max_birthDate) {
  
  n_animals <- n_males + n_females
  birth_dates <- sample(seq(as.Date(min_birthDate), as.Date(max_birthDate), by = "days"), 
                        n_animals, replace = TRUE)
  birth_weights <- rnorm(n_animals, mean = 38, sd = 6)
  
  data_animals <- data.frame(
    ID = 1:n_animals,
    sex = c(rep("male", each = n_males), rep("female", each = n_females)),
    birthDate = birth_dates,
    birthWeight = round(birth_weights)
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
  
      #this data is obtained from growth rates calculated above
  data_merged <- data_merged %>%
    mutate(
      growth_rate = case_when(
        season == "Summer" & sex == "male" ~ rnorm(n(), 0.82, 4.34), 
        season == "Summer" & sex == "female" ~ rnorm(n(), 0.72, 3.61), 
        season == "Autumn" & sex == "male" ~ rnorm(n(), 0.85, 3.82),
        season == "Autumn" & sex == "female" ~ rnorm(n(), 0.63, 3.59),
        season == "Winter" & sex == "male" ~ rnorm(n(), -0.03, 6.93),
        season == "Winter" & sex == "female" ~ rnorm(n(), 0.31, 5.97),
        season == "Spring" & sex == "male" ~ rnorm(n(), 0.23, 4.39),
        season == "Spring" & sex == "female" ~ rnorm(n(), 0.27, 4.76)))
  
  data_merged$age <- as.numeric(data_merged$simDate - data_merged$birthDate + 1)
  data_sim <- subset(data_merged, age > 0)
  
  
  data_sim <- data_sim %>%
    arrange(ID, simDate) %>%
    group_by(ID) %>%
    mutate(simWeight = round(birthWeight + cumsum(growth_rate)))
  
  return(data_sim)  
}
 

# simulation run
n_males <- 50
n_females <- 50
n_days <- 700
min_birthDate <- "2020-09-01"
max_birthDate <- "2021-02-28"

sim_result <- sim_model(n_males, n_females, n_days, min_birthDate, max_birthDate)


ggplot(sim_result, aes(x = age, y = simWeight, color = factor(ID))) +
  geom_line() +
  guides(color = "none")

# ggplot(sim_result, aes(x = age, y = simWeight, color = factor(ID))) +
#   geom_line() +
#   guides(color = "none")


# example animal
example_animal <- subset(sim_result, ID == 1)

ggplot(example_animal, aes(x = simDate, y = simWeight)) +
  geom_line()


# average result
sim_average <- sim_result %>%
  group_by(simDate) %>%
  summarise(avg_weight = mean(simWeight),
            sd_weight = sd(simWeight))

ggplot(sim_average, aes(x = simDate, y = avg_weight)) +
  geom_errorbar(aes(ymin = avg_weight - sd_weight, ymax = avg_weight + sd_weight), color = "grey") +
  geom_line(color = "black")


## Issues with the simulation model: 
  # some 0's and negative values are observed ?????
  # no breed wise difference in growth is included (only sex and seasonal difference is addressed)
    # monthly variation can also be done instead of seasonal
  # growth rate should be normally distributed among the animals, not for a unique ID among days, double check it. 
  # write an algorithm that can perform any number of simulations (Monte-Carlo approach)
    # averaging results form all simulations can provide better prediction, probably 0's and negative values won't appear



# model validation



#########################################################################
## simulation experiment 1, 
#finding what percentage of missing data can still provide 200,400,600-day weights for 100% animals

# splitting the simulated data set into subsets with variable % of missing values

data_sim <- sim_result[, -(5:7)]
colnames(data_sim) <- c("ID", "date", "sex", "birthDate", "age", "weight")


## creating daily weights missing datasets
set.seed(123)

for (i in 1:50) {
  col_name <- paste0("missing_", i)
  data_sim <- data_sim %>%
    group_by(ID) %>%
    mutate(!!col_name := ifelse(runif(n()) <= i / 100, NA, weight)) %>%
    ungroup()
}


# finding 200 day weight collection window
window_200d <- c(max(data_sim$birthDate) + 79, min(data_sim$birthDate) + 299)
window_200d # 2021-05-18" to "2021-06-29", 43 days

data_200d <- subset(data_sim, date >= "2021-05-17" & date <= "2021-06-28")


# count of how many days have 100% animal's weight recorded
count_200d <- data_200d %>%
  group_by(date) %>%
  filter(!is.na(missing_3)) %>%
  filter(all(1:100 %in% ID)) %>%
  ungroup() #




# ## creating weekly weight missing datasets
# data_sim <- sim_result[, -(5:7)]
# colnames(data_sim) <- c("ID", "date", "sex", "birthDate", "age", "weight")
#   
# 
# data_weekly <- data_sim %>%
#   mutate(date = lubridate::ceiling_date(date, unit = "week")) %>%
#   group_by(ID, date) %>%
#   summarize(avg_weekly = round(mean(weight)), .groups = "drop_last")
# 
# data_weekly <- merge(data_weekly, data_sim, by = c("ID", "date"), all.x = TRUE, all.y = FALSE)
# data_weekly <- data_weekly[, -7]
# # data_weekly$birthDate <- as.Date(data_weekly$birthDate, format = "%y-%m-%d")
# 
# 
# set.seed(100)
# 
# for (i in 1:50) {
#   col_name <- paste0("missing_", i)
#   data_weekly <- data_weekly %>%
#     group_by(ID) %>%
#     mutate(!!col_name := ifelse(runif(n()) <= (100 - i) / 100, avg_weekly, NA)) %>%
#     ungroup()
# }
# 
# 
# # finding 200 day weight collection window
# window_200d <- c(max(data_weekly$birthDate, na.rm = TRUE) + 79, min(data_weekly$birthDate, na.rm = TRUE) + 299)
# window_200d # 2021-05-17" to "2021-06-28", 43 days
# 
# data_200d <- subset(data_weekly, date >= "2021-05-17" & date <= "2021-06-28")
# 
# 
# count_200d <- data_200d %>%
#   group_by(date) %>%
#   filter(!is.na(missing_3)) %>%
#   filter(all(1:100 %in% ID)) %>%
#   ungroup() 



##############################################################
# simulation experiment 2
# finding what is the frequency needed to weight animals to get lifetime weights with high (what %) of accuracy

data_sim <- sim_result[, -(5:7)]
colnames(data_sim) <- c("ID", "date", "sex", "birthDate", "age", "weight_daily")

## creating datasets with weights taken 1:20 weeks

data_sim <- data_sim %>%
  group_by(ID) %>%
  mutate(weight_weekly = if_else(row_number() %% 7 == 1, weight_daily, NA_real_),
         weight_2weeks = if_else(row_number() %% 14 == 1, weight_daily, NA_real_),
         weight_4weeks = if_else(row_number() %% 28 == 1, weight_daily, NA_real_),
         weight_6weeks = if_else(row_number() %% 42 == 1, weight_daily, NA_real_),
         weight_8weeks = if_else(row_number() %% 56 == 1, weight_daily, NA_real_),
         weight_10weeks = if_else(row_number() %% 70 == 1, weight_daily, NA_real_),
         weight_12weeks = if_else(row_number() %% 84 == 1, weight_daily, NA_real_),
         weight_14weeks = if_else(row_number() %% 98 == 1, weight_daily, NA_real_),
         weight_16weeks = if_else(row_number() %% 112 == 1, weight_daily, NA_real_),
         weight_18weeks = if_else(row_number() %% 126 == 1, weight_daily, NA_real_),
         weight_20weeks = if_else(row_number() %% 140 == 1, weight_daily, NA_real_)) %>%
  ungroup()

# filling the missing weight days with the last weight record

data_filled <- data_sim %>%
  group_by(ID) %>%
  mutate(weight_weekly = round(na.locf(weight_weekly)),
         weight_2weeks = round(na.locf(weight_2weeks)),
         weight_4weeks = round(na.locf(weight_4weeks)),
         weight_6weeks = round(na.locf(weight_6weeks)),
         weight_8weeks = round(na.locf(weight_8weeks)),
         weight_10weeks = round(na.locf(weight_10weeks)),
         weight_12weeks = round(na.locf(weight_12weeks)),
         weight_14weeks = round(na.locf(weight_14weeks)),
         weight_16weeks = round(na.locf(weight_16weeks)),
         weight_18weeks = round(na.locf(weight_18weeks)),
         weight_20weeks = round(na.locf(weight_20weeks))) %>%
  ungroup()


mape_weekly <- mean(abs((data_filled$weight_daily - data_filled$weight_weekly) / pmax(data_filled$weight_daily, 1))) * 100
mape_weekly

mape_2weeks <- mean(abs((data_filled$weight_daily - data_filled$weight_2weeks) / pmax(data_filled$weight_daily, 1))) * 100
mape_2weeks

mape_4weeks <- mean(abs((data_filled$weight_daily - data_filled$weight_4weeks) / pmax(data_filled$weight_daily, 1))) * 100
mape_4weeks

mape_6weeks <- mean(abs((data_filled$weight_daily - data_filled$weight_6weeks) / pmax(data_filled$weight_daily, 1))) * 100
mape_6weeks

mape_8weeks <- mean(abs((data_filled$weight_daily - data_filled$weight_8weeks) / pmax(data_filled$weight_daily, 1))) * 100
mape_8weeks




# rsq_weekly <- summary(lm(weight_daily ~ weight_weekly, data = data_filled))$r.squared
# rsq_weekly
# 
# rsq_2weeks <- summary(lm(weight_daily ~ weight_2weeks, data = data_filled))$r.squared
# rsq_2weeks
# 
# rsq_4weeks <- summary(lm(weight_daily ~ weight_4weeks, data = data_filled))$r.squared
# rsq_4weeks
# 
# rsq_6weeks <- summary(lm(weight_daily ~ weight_6weeks, data = data_filled))$r.squared
# rsq_6weeks
# 
# rsq_8weeks <- summary(lm(weight_daily ~ weight_8weeks, data = data_filled))$r.squared
# rsq_8weeks
# 
# rsq_10weeks <- summary(lm(weight_daily ~ weight_10weeks, data = data_filled))$r.squared
# rsq_10weeks
# 
# rsq_12weeks <- summary(lm(weight_daily ~ weight_12weeks, data = data_filled))$r.squared
# rsq_12weeks
# 
# rsq_14weeks <- summary(lm(weight_daily ~ weight_14weeks, data = data_filled))$r.squared
# rsq_14weeks
# 
# rsq_16weeks <- summary(lm(weight_daily ~ weight_16weeks, data = data_filled))$r.squared
# rsq_16weeks
# 
# rsq_18weeks <- summary(lm(weight_daily ~ weight_18weeks, data = data_filled))$r.squared
# rsq_18weeks
# 
# rsq_20weeks <- summary(lm(weight_daily ~ weight_20weeks, data = data_filled))$r.squared
# rsq_20weeks



# # assuming animal grow linearly from last weight record to the current recorded day
# data_linear <- data_sim
# 
# data_linear <- data_sim %>%
#   group_by(ID) %>%
#   mutate(weight_weekly = round(na.approx(weight_weekly, na.rm = FALSE)),
#          weight_2weeks = round(na.approx(weight_2weeks, na.rm = FALSE)),
#          weight_4weeks = round(na.approx(weight_4weeks, na.rm = FALSE)),
#          weight_8weeks = round(na.approx(weight_8weeks, na.rm = FALSE)),
#          weight_16weeks = round(na.approx(weight_16weeks, na.rm = FALSE))) %>%
#   ungroup()
# 
# 
# rsq_weekly <- summary(lm(weight_daily ~ weight_weekly, data = data_linear))$r.squared
# rsq_weekly
# 
# rsq_2weeks <- summary(lm(weight_daily ~ weight_2weeks, data = data_linear))$r.squared
# rsq_2weeks
# 
# rsq_4weeks <- summary(lm(weight_daily ~ weight_4weeks, data = data_linear))$r.squared
# rsq_4weeks
# 
# rsq_8weeks <- summary(lm(weight_daily ~ weight_8weeks, data = data_linear))$r.squared
# rsq_8weeks
# 
# rsq_16weeks <- summary(lm(weight_daily ~ weight_16weeks, data = data_linear))$r.squared
# rsq_16weeks











  
  