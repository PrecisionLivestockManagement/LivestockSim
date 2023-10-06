library(DMMongoDB)
library(tidyverse)
library(lubridate)
library(gridExtra)
library(rpart)
library(randomForest)
library(e1071)
library(xgboost)

library(keras)
library(reticulate)
library(tensorflow)

# conda_list()
# conda_create("tf_env")
# use_condaenv("tf_env", required = TRUE)
# install_tensorflow()

# install_keras()
# install.packages("tenserflow")
# install.packages("keras")

# CQIRP_wow <- get_dailywts(property = "CQIRP")
# CQIRP_wow <- CQIRP_wow[, -c(4, 5)]
# CQIRP_wow$Date <- as.Date(CQIRP_wow$Date, format = "%d-%m-%y")
# CQIRP_wow <- subset(CQIRP_wow, Date >= "2018-08-01" & Weight > 0)
# 
# CQIRP_animals <- get_cattle(RFID = unique(CQIRP_wow$RFID), 
#                             fields = c("RFID", "properties.sex",
#                                        "properties.birthDate", "properties.breed",
#                                        "properties.birthWeight"))
# CQIRP_animals <- CQIRP_animals[, -c(1, 3, 6)]
# CQIRP_animals <- subset(CQIRP_animals, sex != "xxxxxx" & breed != "xxxxxx")
# 
# 
# 
# 
# ggplot(CQIRP_wow, aes(x = Date, y = Weight, color = RFID)) +
#   geom_point() +
#   guides(color = "none")
# 
# merged_data <- merge(CQIRP_wow, CQIRP_animals, by = "RFID")
# 
# unique(merged_data$breed)
# 
# write.csv(merged_data, file = "Data/CQIRP_data.csv")

# CQIRP_data <- read.csv(file = "Data/CQIRP_data.csv")
# CQIRP_data <- select(CQIRP_data, -X)



Belmont_stwts <- read.csv(file = "Data/Belmont_staticwts_alltime.csv")
Belmont_DOB <- read.csv(file = "Data/Belmont_DOB.csv")


# changing some column names and removing invalid records
Belmont_stwts <- select(Belmont_stwts, -X)
Belmont_stwts$Date <- as.Date(Belmont_stwts$Date)
Belmont_stwts <- Belmont_stwts[Belmont_stwts$Date != "0014-06-20", ]
Belmont_stwts <- Belmont_stwts[, -4]


Belmont_DOB <- select(Belmont_DOB, -c(X, X_id))
Belmont_DOB <- subset(Belmont_DOB, breed != "xxxxxx" & birthDate != "1970-01-01" & birthDate != "2023-01-01")
Belmont_DOB$breed[Belmont_DOB$breed == "Composite "] <- "Composite"

all_data <- merge(Belmont_stwts, Belmont_DOB, by = "RFID")


# outlier removal of weight records and other processin ####

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


data_merged <- merge(data_clean, Belmont_DOB, by = "RFID")

data_merged$age <- as.numeric(as.Date(data_merged$Date) - as.Date(data_merged$birthDate) + 1)

data_merged <- subset(data_merged, age <= 700)
data_merged <- data_merged[, -c(2, 4, 7, 8)]

data_merged$breed[data_merged$breed == "Brahman"] <- 1
data_merged$breed[data_merged$breed == "Composite"] <- 2
data_merged$breed[data_merged$breed == "BrahxComp"] <- 3
data_merged$breed[data_merged$breed == "Belmont Red"] <- 4

data_merged$sex[data_merged$sex == "female"] <- 0
data_merged$sex[data_merged$sex == "male"] <- 1



# wts_count <- data.frame(RFID = character(), count = numeric())
# 
# for (i in unique(data_merged$RFID)) {
#   count <- data.frame(RFID = i, count = sum(data_merged$RFID == i))
#   wts_count <- rbind(wts_count, count)
# }



# ggplot(data_clean, aes(x = Date, y = Weight)) +
#   geom_point()
# 
ggplot(data_merged, aes(x = age, y = Weight)) +
  geom_point()

ggplot(data_merged, aes(age)) +
  geom_histogram()


# generate complete_data with all animals from age 1 to 1800
ages <- seq(1:700)
new_data <- expand.grid(RFID = unique(data_merged$RFID), age = ages)

complete_data <- merge(new_data, data_merged, by = c("RFID", "age"), all.x = TRUE)

complete_data <- complete_data %>%
  group_by(RFID) %>%
  fill(sex, breed, .direction = "downup") %>%
  ungroup()


#splitting dataset for training (90%) and testing (10%) the models ####

set.seed(123)

selected_RFID <- as.character(unique(complete_data$RFID))

proportion_model <- 0.7

train_animals <- sample(selected_RFID, size = floor(proportion_model * length(selected_RFID)))

test_animals <- setdiff(selected_RFID, train_animals)

data_train <- complete_data %>%
  filter(RFID %in% train_animals)

data_test <- complete_data %>%
  filter(RFID %in% test_animals)


# Multiple linear regression (MLR) ####

model_mlr <- lm(Weight ~ age + sex + breed, data = data_train)

summary(model_mlr)

data_test$predicted_mlr <- predict(model_mlr, newdata = data_test)
data_test$predicted_mlr <- round(data_test$predicted_mlr, 2)


#RMSE_mlr
sqrt(mean((data_test$predicted_mlr - data_test$Weight)^2, na.rm = TRUE))

#mae_mlr
mean(abs(data_test$Weight - data_test$predicted_mlr), na.rm = TRUE)

#mape_mlr
mean(abs((data_test$Weight - data_test$predicted_mlr) / data_test$Weight), na.rm = TRUE) * 100

#rsq_mlr
summary(lm(Weight ~ predicted_mlr, data = data_test))$r.squared



# Decision tree (DT) ####

model_dt <- rpart(Weight ~ age + sex + breed, data = data_train)

summary(model_dt)

data_test$predicted_dt <- predict(model_dt, newdata = data_test)
data_test$predicted_dt <- round(data_test$predicted_dt, 2)

#RMSE_dt
sqrt(mean((data_test$predicted_dt - data_test$Weight)^2, na.rm = TRUE))

#mae_dt
mean(abs(data_test$Weight - data_test$predicted_dt), na.rm = TRUE)

#mape_dt
mean(abs((data_test$Weight - data_test$predicted_dt) / data_test$Weight), na.rm = TRUE) * 100

# rsq_dt
summary(lm(Weight ~ predicted_dt, data = data_test))$r.squared



# random forest (RF) ####

data_rf <- na.omit(data_train[c("Weight", "age", "sex", "breed")])

features <- c("age", "sex", "breed")
target <- "Weight"
train_rf <- data_rf[features]

model_rf <- randomForest(train_rf, data_rf[[target]])

summary(model_rf)

test_rf <- na.omit(data_test[, c("RFID", "age", "sex", "breed")])

data_test$predicted_rf <- predict(model_rf, newdata = test_rf)
data_test$predicted_rf <- round(data_test$predicted_rf, 2)

#RMSE_rf
sqrt(mean((data_test$predicted_rf - data_test$Weight)^2, na.rm = TRUE))

#mae_rf
mean(abs(data_test$Weight - data_test$predicted_rf), na.rm = TRUE)

#mape_rf
mean(abs((data_test$Weight - data_test$predicted_rf) / data_test$Weight), na.rm = TRUE) * 100

# rsq_rf
summary(lm(Weight ~ predicted_rf, data = data_test))$r.squared

# confusionMatrix(data_test$predicted_rf, data_test$Weight)



# Support vector machines (SVM) ####

train_svm <- data_train[c("RFID", "age", "sex", "breed", "Weight")]
model_svm <- svm(Weight ~., data = train_svm, kernel = "radial")

summary(model_svm)

test_svm <- data_test[c("RFID", "age", "sex", "breed")]
data_test$predicted_svm <- predict(model_svm, newdata = test_svm, type = "response")
data_test$predicted_svm <- round(data_test$predicted_svm, 2)


#RMSE_svm
sqrt(mean((data_test$predicted_svm - data_test$Weight)^2, na.rm = TRUE))

#mae_svm
mean(abs(data_test$Weight - data_test$predicted_svm), na.rm = TRUE)

#mape_svm
mean(abs((data_test$Weight - data_test$predicted_svm) / data_test$Weight), na.rm = TRUE) *100

# rsq_svm
summary(lm(Weight ~ predicted_svm, data = data_test))$r.squared



# Extreme Gradient Boosting (XGBoost) ####
train_xgb <- na.omit(data_train)

model_xgb <- xgboost(
  data = data.matrix(train_xgb[, c("age", "sex", "breed")]),
  label = train_xgb$Weight,
  nrounds = 50,  # Number of boosting rounds (adjust as needed)
  objective = "reg:squarederror",  # Use squared error as the objective for regression
  max_depth = 3,  # Maximum tree depth (adjust as needed)
  eta = 0.3  # Learning rate (adjust as needed)
)

summary(model_xgb)

data_test$predicted_xgb <- predict(model_xgb, newdata = data.matrix(data_test[, c("age", "sex", "breed")]))
data_test$predicted_xgb <- round(data_test$predicted_xgb, 2)


#RMSE_xgb
sqrt(mean((data_test$predicted_xgb - data_test$Weight)^2, na.rm = TRUE))

#mae_xgb
mean(abs(data_test$Weight - data_test$predicted_xgb), na.rm = TRUE)

#mape_xgb
mean(abs((data_test$Weight - data_test$predicted_xgb) / data_test$Weight), na.rm = TRUE) * 100

# rsq_xgb
summary(lm(Weight ~ predicted_xgb, data = data_test))$r.squared


# Long short term memory (LSTM) ####

train_lstm <- na.omit(data_train[c("RFID", "age", "sex", "breed", "Weight")])
test_lstm <- na.omit(data_test[, c("RFID", "age", "sex", "breed", "Weight")])

sequence_length <- 100

n_features <- c("age", "sex", "breed", "Weight")

# Define and prepare sequences for training and testing
sequences_train <- list()
for (i in 1:(nrow(train_lstm) - sequence_length + 1)) {
  sequence <- train_lstm[i:(i + sequence_length - 1), ]
  sequences_train[[i]] <- as.matrix(sequence[, n_features])
}

sequences_test <- list()
for (i in 1:(nrow(test_lstm) - sequence_length + 1)) {
  sequence <- test_lstm[i:(i + sequence_length - 1), ]
  sequences_test[[i]] <- as.matrix(sequence[, n_features])
}

# Create the LSTM model
model_lstm <- keras_model_sequential() %>%
  layer_lstm(units = 50, input_shape = c(sequence_length, length(n_features))) %>%
  layer_dense(units = 1)  # Output layer for regression

# Compile the model
model_lstm %>% compile(
  loss = "mean_squared_error",
  optimizer = "rmsprop"
)

# Prepare training data
x_train <- array_reshape(unlist(sequences_train), c(length(sequences_train), sequence_length, length(n_features)))
y_train <- data_train$Weight[sequence_length:(nrow(data_train) - sequence_length + 1)]

# Training of the model
model_lstm %>% fit(
  x_train,  # Training data
  y_train,  # Training labels
  epochs = 10,
  batch_size = 32,
  # validation_data = list(x_val, y_val)  # Define x_val and y_val
)

# Evaluate the model
x_test <- array_reshape(unlist(sequences_test), c(length(sequences_test), sequence_length, length(n_features)))
y_test <- data_test$Weight[sequence_length:(nrow(data_test) - sequence_length + 1)]
mse <- model_lstm %>% evaluate(x_test, y_test)
cat("Mean Squared Error:", mse$loss, "\n")

# Make predictions
data_test$predicted_lstm <- model_lstm %>% predict(x_test)




# model predictions on complete_data ####

# data_predicted <- complete_data
# 
# data_predicted$predicted_mlr <- predict(model_mlr, newdata = data_predicted)
# data_predicted$predicted_mlr <- round(data_predicted$predicted_mlr, 2)
# 
# data_predicted$predicted_dt <- predict(model_dt, newdata = data_predicted)
# data_predicted$predicted_dt <- round(data_predicted$predicted_dt, 2)
# 
# 
# predict_rf <- na.omit(data_predicted[, c("RFID", "age", "sex", "breed")])
# data_predicted$predicted_rf <- predict(model_rf, newdata = predict_rf)
# data_predicted$predicted_rf <- round(data_predicted$predicted_rf, 2)
# 
# predict_svm <- data_predicted[c("RFID", "age", "sex", "breed")]
# data_predicted$predicted_svm <- predict(model_svm, newdata = predict_svm)
# data_predicted$predicted_svm <- round(data_predicted$predicted_svm, 2)
# 
# predict_xgb <- data_predicted[c("age", "sex", "breed")]
# data_predicted$predicted_xgb <- predict(model_xgb, newdata = data.matrix(predict_xgb))
# data_predicted$predicted_xgb <- round(data_predicted$predicted_xgb, 2)


# plotting results ####

ggplot() +
  geom_histogram(data = data_merged, aes(age), binwidth = 10, color = "black", fill = "grey") +
  scale_x_continuous(name = "Age (days)", breaks = seq(0, 700, by = 100), limits = c(0, 700)) +
  scale_y_continuous(name = "No. of Weights", breaks = seq(0, 120, by = 20))


scatter_mlr <- ggplot(data_test, aes(x = Weight, y = predicted_mlr)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue", formula = y ~ x) +
  labs(x = "Actual weights (kg)", y = "Predicted weights (kg)") +
  ggtitle("(a)") + theme(plot.title = element_text(hjust = 0.5)) 

scatter_dt <- ggplot(data_test, aes(x = Weight, y = predicted_dt)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue", formula = y ~ x) +
  labs(x = "Actual weights (kg)", y = "Predicted weights (kg)") +
  ggtitle("(b)") + theme(plot.title = element_text(hjust = 0.5))

scatter_rf <- ggplot(data_test, aes(x = Weight, y = predicted_rf)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue", formula = y ~ x) +
  labs(x = "Actual weights (kg)", y = "Predicted weights (kg)") +
  ggtitle("(c)") + theme(plot.title = element_text(hjust = 0.5))

scatter_svm <- ggplot(data_test, aes(x = Weight, y = predicted_svm)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue", formula = y ~ x) +
  labs(x = "Actual weights (kg)", y = "Predicted weights (kg)") +
  ggtitle("(d)") + theme(plot.title = element_text(hjust = 0.5)) 

scatter_xgb <- ggplot(data_test, aes(x = Weight, y = predicted_xgb)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue", formula = y ~ x) +
  labs(x = "Actual weights (kg)", y = "Predicted weights (kg)") +
  ggtitle("(e)") + theme(plot.title = element_text(hjust = 0.5))

grid.arrange(scatter_mlr, scatter_dt, scatter_rf, scatter_svm, scatter_xgb, nrow = 3)



data_avg <- data_test %>%
  mutate(age = floor(age / 10) * 10) %>%
  group_by(age) %>%
  summarise(wts_avg = round(mean(Weight, na.rm = TRUE), 2),
            mlr_avg = round(mean(predicted_mlr, na.rm = TRUE), 2),
            dt_avg = round(mean(predicted_dt, na.rm = TRUE), 2),
            rf_avg = round(mean(predicted_rf, na.rm = TRUE), 2),
            svm_avg = round(mean(predicted_svm, na.rm = TRUE), 2),
            xgb_avg = round(mean(predicted_xgb, na.rm = TRUE), 2),)


graph_mlr <- ggplot() +
  geom_line(data = data_avg[!is.nan(data_avg$wts_avg), ], 
            aes(x = age, y = wts_avg, linetype = "Actual weights")) +
  geom_line(data = data_avg, 
            aes(x = age, y = mlr_avg, linetype = "MLR prediction")) +
  labs(linetype = NULL) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.75, 0.20)) +
  scale_x_continuous(name = "Age (days)", breaks = seq(0, 700, by = 100), limits = c(0, 700)) +
  scale_y_continuous(name = "Weight (kg)", breaks = seq(0, 500, by = 100), limits = c(0, 500)) +
  scale_linetype_manual(values = c("Actual weights" = "solid",
                                   "MLR prediction" = "longdash")) +
  ggtitle("(a)")

graph_dt <- ggplot() +
  geom_line(data = data_avg[!is.nan(data_avg$wts_avg), ], 
            aes(x = age, y = wts_avg, linetype = "Actual weights")) +
  geom_line(data = data_avg, 
            aes(x = age, y = dt_avg, linetype = "DT prediction")) +
  labs(linetype = NULL) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.75, 0.20)) +
  scale_x_continuous(name = "Age (days)", breaks = seq(0, 700, by = 100), limits = c(0, 700)) +
  scale_y_continuous(name = "Weight (kg)", breaks = seq(0, 500, by = 100), limits = c(0, 500)) +
  scale_linetype_manual(values = c("Actual weights" = "solid",
                                   "DT prediction" = "longdash")) +
  ggtitle("(b)")

graph_rf <- ggplot() +
  geom_line(data = data_avg[!is.nan(data_avg$wts_avg), ], 
            aes(x = age, y = wts_avg, linetype = "Actual weights")) +
  geom_line(data = data_avg, 
            aes(x = age, y = rf_avg, linetype = "RF prediction")) +
  labs(linetype = NULL) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.75, 0.20)) +
  scale_x_continuous(name = "Age (days)", breaks = seq(0, 700, by = 100), limits = c(0, 700)) +
  scale_y_continuous(name = "Weight (kg)", breaks = seq(0, 500, by = 100), limits = c(0, 500)) +
  scale_linetype_manual(values = c("Actual weights" = "solid",
                                   "RF prediction" = "longdash")) +
  ggtitle("(c)")

graph_svm <- ggplot() +
  geom_line(data = data_avg[!is.nan(data_avg$wts_avg), ], 
            aes(x = age, y = wts_avg, linetype = "Actual weights")) +
  geom_line(data = data_avg, 
            aes(x = age, y = svm_avg, linetype = "SVM prediction")) +
  labs(linetype = NULL) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.75, 0.20)) +
  scale_x_continuous(name = "Age (days)", breaks = seq(0, 700, by = 100), limits = c(0, 700)) +
  scale_y_continuous(name = "Weight (kg)", breaks = seq(0, 500, by = 100), limits = c(0, 500)) +
  scale_linetype_manual(values = c("Actual weights" = "solid",
                                   "SVM prediction" = "longdash")) +
  ggtitle("(d)")

graph_xgb <- ggplot() +
  geom_line(data = data_avg[!is.nan(data_avg$wts_avg), ], 
            aes(x = age, y = wts_avg, linetype = "Actual weights")) +
  geom_line(data = data_avg, 
            aes(x = age, y = xgb_avg, linetype = "XGB prediction")) +
  labs(linetype = NULL) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.75, 0.20)) +
  scale_x_continuous(name = "Age (days)", breaks = seq(0, 700, by = 100), limits = c(0, 700)) +
  scale_y_continuous(name = "Weight (kg)", breaks = seq(0, 500, by = 100), limits = c(0, 500)) +
  scale_linetype_manual(values = c("Actual weights" = "solid",
                                   "XGB prediction" = "longdash")) +
  ggtitle("(e)")

grid.arrange(graph_mlr, graph_dt, graph_rf, graph_svm, graph_xgb, nrow = 3)

# ggplot() +
#   geom_line(data = data_avg[!is.nan(data_avg$wts_avg), ], 
#             aes(x = age, y = wts_avg, linetype = "Observed weights")) +
#   geom_line(data = data_avg, 
#             aes(x = age, y = mlr_avg, linetype = "MLR prediction")) +
#   geom_line(data = data_avg, 
#             aes(x = age, y = dt_avg, linetype = "DT prediction")) +
#   geom_line(data = data_avg, 
#             aes(x = age, y = rf_avg, linetype = "RF prediction")) +
#   labs(linetype = NULL) + 
#   theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.75, 0.28)) +
#   scale_x_continuous(name = "Age (days)", breaks = seq(0, 900, by = 100), limits = c(0, 900)) +
#   scale_y_continuous(name = "Weight (kg)", breaks = seq(0, 600, by = 100), limits = c(0, 600)) +
#   scale_linetype_manual(values = c("Observed weights" = "solid",
#                                    "MLR prediction" = "dotted",
#                                    "DT prediction" = "dashed",
#                                    "RF prediction" = "longdash"))



















