# #####
# library(DMMongoDB)
# library(tidyverse)
# library(lubridate)
# library(gridExtra)
# library(zoo)
# library(DescTools)
# library(xgboost)



#' growth traits estimator
#'
#' @param WOW_data
#'
#' @return
#' @export
#'
#' @examples
#'

growth_traits <- function (WOW_data) {

  WOW_data$Date <- as.Date(WOW_data$Date)
  WOW_data$BirthDate <- as.Date(WOW_data$BirthDate)

  WOW_data <- subset(WOW_data, Weight >= 20)

  # averaging if more than one record in a day
  WOW_data_1 <- WOW_data %>%
    group_by(ID, Date) %>%
    summarise(Weight = mean(Weight))

  # #pick animals with at least 5 days of Weights
  # WOW_data_1 <- WOW_data_1 %>%
  #   group_by(ID) %>%
  #   filter(n_distinct(Weight) >= 10)

  custom_mean <- function(x) {
    if (sum(!is.na(x)) >= 5) {
      return(mean(x, na.rm = TRUE))
    } else {
      return(NA)
    }
  }

  #Outlier removal
  WOW_data_1 <- WOW_data_1 %>%
    group_by(ID) %>%
    complete(Date = seq(min(Date), max(Date), by = "day"))

  WOW_data_1 <- WOW_data_1 %>%
    group_by(ID) %>%
    arrange(Date) %>%
    mutate(RefWeight = round(rollapply(Weight, width = 15, align = 'center', FUN = custom_mean, fill = NA), 2)) %>%
    ungroup()

  WOW_data_out <- WOW_data_1 %>%
    mutate(Weight = ifelse(Weight < RefWeight - 50 | Weight > RefWeight + 50, NA, Weight))

  # smoothing using 15 day rolling average
  data_smooth <- WOW_data_out %>%
    group_by(ID) %>%
    arrange(Date) %>%
    mutate(Weight = round(rollapply(Weight, width = 15, align = 'center', FUN = custom_mean, fill = NA), 2)) %>%
    ungroup()

  data_smooth <- subset(data_smooth, !is.na(Weight))


  #
  birth_dates <- WOW_data[ , c("ID", "BirthDate")] %>%
    distinct()

  data_merged <- merge(data_smooth, birth_dates, by = "ID")
  data_merged$Age <- as.numeric(data_merged$Date - data_merged$BirthDate + 1)


  # 200 day weight calculation
  day200_data <- subset(data_merged, Age >= 80 & Age <= 300)


  day200_data$period <- as.numeric(cut(day200_data$Age, breaks = seq(80, 300, by = 15),
                                       labels = 1:(length(seq(80, 300, by = 15)) - 1)))

  day200_data_filtered <- day200_data %>%
    group_by(ID, period) %>%
    filter(length(Weight) >= 5)

  day200_data_filtered_1 <- day200_data_filtered %>%
    group_by(ID) %>%
    mutate(target_period = period[which.min(abs(period - 200))]) %>%
    filter(period == target_period)


  day200_breedplan <- day200_data_filtered_1 %>%
    group_by(ID) %>%
    summarise(Weight_200d = round(mean(Weight)), Date_200d = mean(Date))


  # 400 day weight calculation
  day400_data <- subset(data_merged, Age >= 301 & Age <= 500)


  day400_data$period <- as.numeric(cut(day400_data$Age, breaks = seq(301, 500, by = 15),
                                       labels = 1:(length(seq(301, 500, by = 15)) - 1)))

  day400_data_filtered <- day400_data %>%
    group_by(ID, period) %>%
    filter(length(Weight) >= 5)

  day400_data_filtered_1 <- day400_data_filtered %>%
    group_by(ID) %>%
    mutate(target_period = period[which.min(abs(period - 400))]) %>%
    filter(period == target_period)


  day400_breedplan <- day400_data_filtered_1 %>%
    group_by(ID) %>%
    summarise(Weight_400d = round(mean(Weight)), Date_400d = mean(Date))


  # 600 day weight calculation
  day600_data <- subset(data_merged, Age >= 501 & Age <= 900)


  day600_data$period <- as.numeric(cut(day600_data$Age, breaks = seq(501, 900, by = 15),
                                       labels = 1:(length(seq(501, 900, by = 15)) - 1)))

  day600_data_filtered <- day600_data %>%
    group_by(ID, period) %>%
    filter(length(Weight) >= 5)

  day600_data_filtered_1 <- day600_data_filtered %>%
    group_by(ID) %>%
    mutate(target_period = period[which.min(abs(period - 600))]) %>%
    filter(period == target_period)


  day600_breedplan <- day600_data_filtered_1 %>%
    group_by(ID) %>%
    summarise(Weight_600d = round(mean(Weight)), Date_600d = mean(Date))


  data_output <- merge(merge(day200_breedplan, day400_breedplan, by = "ID", all.x = TRUE, all.y = TRUE),
                       day600_breedplan, by = "ID", all.x = TRUE, all.y = TRUE)

  return(data_output)
}


# # Example run
# data_input <- read.csv("https://raw.githubusercontent.com/PrecisionLivestockManagement/LivestockSim/refs/heads/main/Data/simulated_data.csv")
#
# growthtraits_output <- growth_traits(data_input)

