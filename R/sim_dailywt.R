# library(DMMongoDB)
# library(tidyverse)
# library(lubridate)
# library(gridExtra)
# library(zoo)
# library(DescTools)
# library(xgboost)

##############################################

#' Cattle daily weight simulator
#'
#' @param n_males_BelmontRed
#' @param n_females_BelmontRed
#' @param n_males_Brahman
#' @param n_females_Brahman
#' @param n_males_Composite
#' @param n_females_Composite
#' @param n_males_Unknown
#' @param n_females_Unknown
#' @param n_days
#' @param min_birthDate
#' @param max_birthDate
#' @param weather_data CSV file containing Date, daily_rain, max_temp and min_temp columns
#'
#' @return
#' @export
#'
#' @examples
#'
sim_dailywt <- function(n_males_BelmontRed, n_females_BelmontRed,
                      n_males_Brahman, n_females_Brahman,
                      n_males_Composite, n_females_Composite,
                      n_males_Unknown, n_females_Unknown,
                      n_days, min_birthDate, max_birthDate, weather_data) {


  # all animals
  n_animals <- n_males_BelmontRed + n_females_BelmontRed + n_males_Brahman + n_females_Brahman +
    n_males_Composite + n_females_Composite + n_males_Unknown + n_females_Unknown

  # birth dates are distributed randomly
  birth_dates <- sample(seq(as.Date(min_birthDate), as.Date(max_birthDate), by = "days"),
                        n_animals, replace = TRUE)

  # creating data-frame with required predictor variables
  data_animals <- data.frame(
    ID = 1:n_animals,
    breed = c(rep("Belmont Red", each = n_males_BelmontRed + n_females_BelmontRed),
              rep("Brahman", each = n_males_Brahman + n_females_Brahman),
              rep("composite", each = n_males_Composite + n_females_Composite),
              rep("Unknown", each = n_males_Unknown + n_females_Unknown)),
    sex = c(c(rep("male", each = n_males_BelmontRed), rep("female", each = n_females_BelmontRed)),
            c(rep("male", each = n_males_Brahman), rep("female", each = n_females_Brahman)),
            c(rep("male", each = n_males_Composite), rep("female", each = n_females_Composite)),
            c(rep("male", each = n_males_Unknown), rep("female", each = n_females_Unknown))),
    birthDate = birth_dates
  )

  # weather data processing
  weather_data$Date <- as.Date(weather_data$Date)
  weather_data$avg_temp <- round((weather_data$max_temp + weather_data$min_temp) / 2, 1)

  weather_data <- weather_data %>%
    group_by(month = format(Date, "%Y-%m")) %>%
    mutate(monthly_rain = round(sum(daily_rain), 2))

  weather_data <- weather_data %>%
    group_by(month = format(Date, "%Y-%m")) %>%
    mutate(monthly_temp = round(mean(avg_temp), 2))

  weather_monthly <- weather_data[, -c(2:6)]
  weather_data$weather_factor <- floor((weather_data$monthly_rain + weather_data$monthly_temp) / 50) + 1

  # no. of days to simulate (simulation runs until the youngest animals reaches n_days)
  simulation_dates <- seq(min(birth_dates), max(birth_dates) + n_days, by = "days")

  data_new <- expand.grid(ID = unique(data_animals$ID), Date = simulation_dates)

  data_merged <- merge(merge(data_new, data_animals, by = "ID", all.x = TRUE), weather_data, by = "Date")


  data_merged$age <- as.numeric(data_merged$Date - data_merged$birthDate + 1)

  data_sim <- subset(data_merged, age > 0)


  #####
  Belmont_dailywts <- read.csv("https://raw.githubusercontent.com/PrecisionLivestockManagement/LivestockSim/main/Data/Belmont_dailywts.csv")
  Belmont_dailywts$stationname <- "Belmont"
  Belmont_staticwts <- read.csv("https://raw.githubusercontent.com/PrecisionLivestockManagement/LivestockSim/refs/heads/main/Data/Belmont_staticwts.csv")
  Belmont_animals <- read.csv("https://raw.githubusercontent.com/PrecisionLivestockManagement/LivestockSim/refs/heads/main/Data/Belmont_animals.csv")

  Tremere_dailywts <- read.csv("https://raw.githubusercontent.com/PrecisionLivestockManagement/LivestockSim/refs/heads/main/Data/Tremere_dailywts.csv")
  Tremere_dailywts$stationname <- "Tremere"
  Tremere_staticwts <- read.csv("https://raw.githubusercontent.com/PrecisionLivestockManagement/LivestockSim/refs/heads/main/Data/Tremere_staticwts.csv")
  Tremere_animals <- read.csv("https://raw.githubusercontent.com/PrecisionLivestockManagement/LivestockSim/refs/heads/main/Data/Tremere_animals.csv")

  # combine Belmont and Tremere data in a same dataframe
  dailywts_all <- rbind(Belmont_dailywts, Tremere_dailywts)
  staticwts_all <- rbind(Belmont_staticwts, Tremere_staticwts)
  animals_all <- rbind(Belmont_animals, Tremere_animals)

  # pre-processing of dailywt data
  dailywts_all <- dailywts_all[, -c(1, 5, 6)]
  dailywts_all$Date <- as.Date(dailywts_all$Date)
  colnames(dailywts_all)[colnames(dailywts_all) == "Weight"] <- "dailywt"


  # pre-processing staticwt data
  staticwts_all <- staticwts_all[, -1]
  staticwts_all$Date <- as.Date(staticwts_all$Date)
  colnames(staticwts_all)[colnames(staticwts_all) == "Weight"] <- "staticwt"
  staticwts_all <- staticwts_all[staticwts_all$Date != "0014-06-19", ]

  # pre-processing of animal data
  animals_all <- animals_all[, -c(1, 2, 7)]
  animals_all <- subset(animals_all, birthDate >= "2010-09-01" & birthDate != "2023-01-01" & sex != "xxxxxx")
  animals_all <- animals_all %>%
    mutate(birthWeight = ifelse(birthWeight == 0 | is.na(birthWeight), 38, birthWeight))
  animals_all$birthWeight <- round(as.numeric(animals_all$birthWeight))


  # selecting RFID's with all required variables available
  common_RFID <- intersect(intersect(unique(dailywts_all$RFID), unique(staticwts_all$RFID)), unique(animals_all$RFID))

  animals_selected <- animals_all %>%
    filter(RFID %in% common_RFID) %>%
    distinct()

  dailywts_selected <- dailywts_all %>%
    filter(RFID %in% common_RFID)

  staticwts_selected <- staticwts_all %>%
    filter(RFID %in% common_RFID)


  # adding birthWeigt to the static weights record
  animals_birth_info <- animals_selected
  colnames(animals_birth_info)[colnames(animals_birth_info) == "birthDate"] <- "Date"

  staticwts_selected <- merge(staticwts_selected, animals_birth_info, by = c("RFID", "stationname", "Date"), all.x = TRUE, all.y =  TRUE)
  staticwts_selected$staticwt <- ifelse(!is.na(staticwts_selected$birthWeight), staticwts_selected$birthWeight, staticwts_selected$staticwt)
  staticwts_selected <- staticwts_selected[, -c(5:7)]

  # outlier removal of static weights
  staticwts_out <- subset(staticwts_selected, staticwt >= 19)


  # averaging staticwts if more than one record in a day
  staticwts_data <- staticwts_out %>%
    group_by(RFID, Date, stationname) %>%
    summarise(staticwt = round(mean(staticwt), 2)) %>%
    ungroup()

  # #selecting animals with at least 2 static weight records
  # staticwts_data <- staticwts_data %>%
  #   group_by(RFID) %>%
  #   filter(n_distinct(staticwt) >=2)


  # ggplot(staticwts_data, aes(x = Date, y = staticwt, color = RFID)) +
  #   geom_point() + guides(color = "none")

  # using static wts as refrence weights
  staticwts_ref <- merge(staticwts_data, animals_selected, by = c("RFID", "stationname"))
  staticwts_ref$age <- as.numeric(as.Date(staticwts_ref$Date) - as.Date(staticwts_ref$birthDate) + 1)
  staticwts_ref <- subset(staticwts_ref, age > 0)

  staticwts_ref <- staticwts_ref %>%
    group_by(RFID) %>%
    complete(age = 1:max(age))


  staticwts_ref <- staticwts_ref %>%
    group_by(RFID) %>%
    fill(sex, breed, birthDate, birthWeight, stationname, .direction = "downup") %>%
    ungroup()

  staticwts_ref$Date <- as.Date(staticwts_ref$birthDate) + staticwts_ref$age - 1

  staticwts_ref <- staticwts_ref %>%
    mutate(staticwt = if_else(age == 1, birthWeight, staticwt))

  # linear interpolation of ref weights
  staticwts_ref <- staticwts_ref %>%
    group_by(RFID) %>%
    arrange(age) %>%
    mutate(refWeight = round(na.approx(staticwt, na.rm = TRUE), 2))


  # outlier removal from dailywts
  dailywts_selected_1 <- subset(dailywts_selected, dailywt >= 19)

  dailywts_raw <- merge(dailywts_selected_1, staticwts_ref, by = c("RFID", "Date", "stationname"), all.x = TRUE, all.y = TRUE)


  dailywts_30below <- subset(dailywts_raw, age <= 30)
  dailywts_30below <- subset(dailywts_30below, dailywt < 150)

  dailywts_30above <- subset(dailywts_raw, age >= 31)

  dailywts_raw <- rbind(dailywts_30below, dailywts_30above)



  # dailywts_raw <- dailywts_raw %>%
  #   group_by(age) %>%
  #   mutate(avg_refWeight = round(mean(refWeight), 2),
  #          sd_refWeight = round(sd(refWeight), 2))

  dailywts_raw$outlier <- ifelse(dailywts_raw$dailywt < (dailywts_raw$refWeight - 50) |
                                   dailywts_raw$dailywt > (dailywts_raw$refWeight + 50), dailywts_raw$dailywt, NA)

  dailywts_raw$dailywt <- ifelse(dailywts_raw$dailywt >= dailywts_raw$refWeight - 50 &
                                   dailywts_raw$dailywt <= dailywts_raw$refWeight + 50, dailywts_raw$dailywt, NA)

  dailywts_out <- subset(dailywts_raw, !is.na(dailywt))

  # dailywts_out <- subset(dailywts_raw, dailywt >= avg_refWeight - (2 * sd_refWeight) &
  #                          dailywt <= avg_refWeight + (2 * sd_refWeight))

  # averaging dailywts if more than one record in a day
  dailywts_data <- dailywts_out %>%
    group_by(RFID, Date, stationname) %>%
    summarise(dailywt = round(mean(dailywt), 2)) %>%
    ungroup()

  # weather data processing
  Belmont_weather <- read.csv("https://raw.githubusercontent.com/PrecisionLivestockManagement/LivestockSim/refs/heads/main/Data/WeatherData_39083.csv")
  Belmont_weather <- Belmont_weather[, c("YYYY.MM.DD", "daily_rain", "max_temp", "min_temp")]
  colnames(Belmont_weather)[colnames(Belmont_weather) == "YYYY.MM.DD"] <- 'Date'
  Belmont_weather$Date <- as.Date(Belmont_weather$Date)

  Belmont_weather$avg_temp <- round((Belmont_weather$max_temp + Belmont_weather$min_temp) / 2, 1)

  Belmont_weather <- Belmont_weather %>%
    group_by(month = format(Date, "%Y-%m")) %>%
    mutate(monthly_rain = round(sum(daily_rain), 2))

  Belmont_weather <- Belmont_weather %>%
    group_by(month = format(Date, "%Y-%m")) %>%
    mutate(monthly_temp = round(mean(avg_temp), 2))

  Belmont_weather_monthly <- Belmont_weather[, -c(2:6)]
  Belmont_weather_monthly$stationname <- "Belmont"


  # Tremere weather data processing (weather station is 50 km away)
  Tremere_weather <- read.csv("https://raw.githubusercontent.com/PrecisionLivestockManagement/LivestockSim/refs/heads/main/Data/WeatherData_39089_TR.csv")
  Tremere_weather <- Tremere_weather[, c("YYYY.MM.DD", "daily_rain", "max_temp", "min_temp")]
  colnames(Tremere_weather)[colnames(Tremere_weather) == "YYYY.MM.DD"] <- 'Date'
  Tremere_weather$Date <- as.Date(Tremere_weather$Date)

  Tremere_weather$avg_temp <- round((Tremere_weather$max_temp + Tremere_weather$min_temp) / 2, 1)

  Tremere_weather <- Tremere_weather %>%
    group_by(month = format(Date, "%Y-%m")) %>%
    mutate(monthly_rain = round(sum(daily_rain), 2))

  Tremere_weather <- Tremere_weather %>%
    group_by(month = format(Date, "%Y-%m")) %>%
    mutate(monthly_temp = round(mean(avg_temp), 2))

  Tremere_weather_monthly <- Tremere_weather[, -c(2:6)]
  Tremere_weather_monthly$stationname <- "Tremere"

  weather_data_monthly <- rbind(Belmont_weather_monthly, Tremere_weather_monthly)
  weather_data_monthly$weather_factor <- floor((weather_data_monthly$monthly_rain + weather_data_monthly$monthly_temp) / 50) + 1
  weather_data_monthly <- weather_data_monthly[, -c(2, 3)]


  # complete_data
  complete_data <- merge(merge(dailywts_data, animals_selected, by = c("RFID", "stationname")), weather_data_monthly, by = c("stationname", "Date"))

  complete_data$age <- as.numeric(as.Date(complete_data$Date) - as.Date(complete_data$birthDate) + 1)

  complete_data <- complete_data %>%
    mutate(breed = recode(breed, "Composite " = "Composite",
                          "BR" = "Belmont Red",
                          "BrahxComp" = "Unknown",
                          "BJ" = "Unknown",
                          "TI" = "Unknown",
                          "SE" = "Unknown",
                          "xxxxxx" = "Unknown"))



  # splitting complete_data for model training validation and testing

  data_test_1 <- subset(complete_data, stationname == "Belmont" & (breed == "Belmont Red" | breed == "Composite") &
                          birthDate >= "2018-09-01" & birthDate <= "2019-02-28") #
  data_test_2 <- subset(complete_data, stationname == "Belmont" & (breed == "Brahman" | breed == "Unknown") &
                          birthDate >= "2020-09-01" & birthDate <= "2021-02-28")#

  data_model <- complete_data %>%
    filter(!(RFID %in% data_test_1$RFID) & !(RFID %in% data_test_2$RFID))

  model_RFID <- unique(data_model$RFID)

  set.seed(0)
  train_RFID <- sample(model_RFID, size = floor(0.8 * length(model_RFID)))
  validation_RFID <- setdiff(model_RFID, train_RFID)

  data_train <- data_model %>%
    filter(RFID %in% train_RFID)

  data_validation <- data_model %>%
    filter(RFID %in% validation_RFID)


  data_train %>%
    group_by(breed, sex) %>%
    summarise(count = n_distinct(RFID))

  data_validation %>%
    group_by(breed, sex) %>%
    summarise(count = n_distinct(RFID))

  data_test_1 %>%
    group_by(breed, sex) %>%
    summarise(count = n_distinct(RFID))

  data_test_2 %>%
    group_by(breed, sex) %>%
    summarise(count = n_distinct(RFID))

  # using the best hyperparameters obtained from random search above
  # set.seed(10)
  model_xgb <- xgboost(
    data = data.matrix(data_train[, c("age", "sex", "breed", "weather_factor")]),
    label = data_train$dailywt,
    max_depth = 4,
    learning_rate = 0.1,
    nthread = 6,
    nrounds = 100,
    subsample = 0.8,
    colsample_bytree = 1,
    objective = "reg:squarederror",
    verbose = 0
  )

  ####################


  # using the trained model for prediction
  set.seed(3)
  data_sim$Weight <- round(predict(model_xgb, newdata = data.matrix(data_sim[, c("age", "sex", "breed", "weather_factor")])), 2)


  return(data_sim[, -c(6:14)])
}


# # simulation run
# weather_data_Belmont <- read.csv("https://raw.githubusercontent.com/PrecisionLivestockManagement/LivestockSim/refs/heads/main/Data/WeatherData_example.csv")
#
# output_data <- sim_dailywt(n_females_BelmontRed = 10, n_males_BelmontRed = 10,
#                            n_females_Brahman = 10, n_males_Brahman = 10,
#                            n_females_Composite = 10, n_males_Composite = 10,
#                            n_females_Unknown = 0, n_males_Unknown = 0,
#                            n_days = 500, min_birthDate = "2018-09-01", max_birthDate = "2019-02-01",
#                            weather_data = weather_data_Belmont)
