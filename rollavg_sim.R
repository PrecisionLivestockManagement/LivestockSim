library(DMMongoDB)
library(tidyverse)
library(lubridate)
library(changepoint)
library(ggchangepoint)
library(gridExtra)
library(cowplot)
library(patchwork)

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
staticwts_raw <- staticwts_all[staticwts_all$staticwt > 0, ]

# model development ####
# selecting weight records between 2023-03-03 to 2023-03-31, 
staticwts_model <- staticwts_raw[staticwts_raw$Date >= "2023-03-03" & staticwts_raw$Date <= "2023-03-31", ]

# staticwt_count <- data.frame(RFID = character(), count = numeric())
# 
# for (i in unique(staticwts_model$RFID)) {
#   count <- data.frame(RFID = i, count = sum(staticwts_model$RFID == i))
#   staticwt_count <- rbind(staticwt_count, count)
# }
# 
# staticwt_count <- staticwt_count[order(-staticwt_count$count), ]
# 
# top_100_RFIDs <- head(staticwt_count, 100)
# 
# 
# staticwts_selected <- staticwts_model %>%
#   filter(RFID %in% top_100_RFIDs$RFID)

staticwts_selected <- staticwts_model %>%
  group_by(RFID) %>%
  filter(any(Date == as.Date("2023-03-03")) & any(Date == as.Date("2023-03-31")))

# selecting wow between 2023-02-16 to 2023-04-15 (15 day prior and 15 days after of static weight days) for animals selected at staticwts_filtered
dailywts_model <- dailywts_raw[dailywts_raw$Date >= "2023-02-16" & dailywts_raw$Date <= "2023-04-15", ]

dailywts_filtered <- dailywts_model %>%
  filter(RFID %in% unique(staticwts_selected$RFID))


# linear interpolation to get the static weights for the missing days
dates_staticwt <- seq(as.Date("2023-03-03"), as.Date("2023-03-31"), by = "day")
new_staticwts <- expand.grid(RFID = unique(staticwts_selected$RFID), Date = dates_staticwt)

merged_staticwts <- merge(new_staticwts, staticwts_selected, by = c("RFID", "Date"), all.x = TRUE)

staticwts_splited <- split(merged_staticwts, f = merged_staticwts$RFID)

est_staticwts_list <- list()

for (i in seq_along(staticwts_splited)) {
  est_staticwts_list[[i]] <- with(staticwts_splited[[i]], approx(x = Date, y = staticwt, xout = dates_staticwt)$y)
}

staticwts_selected <- cbind(merged_staticwts, est_staticwts = unlist(est_staticwts_list))
#staticwts_selected <- staticwts_selected[, -c(3:4)]
staticwts_selected$est_staticwts <- round(staticwts_selected$est_staticwts, 2)


# outlier removal from WOW data
dailywts_filtered <- dailywts_filtered %>%
  group_by(RFID) %>%
  mutate(mean_dailywt = round(mean(dailywt), 2), sd_dailywt = round(sd(dailywt), 2))


dailywts_out <- data.frame()

for (i in unique(dailywts_filtered$RFID)) {
  animal_data <- subset(dailywts_filtered, RFID == i)
  
  lower_limit <- animal_data$mean_dailywt - 2 * animal_data$sd_dailywt
  upper_limit <- animal_data$mean_dailywt + 2 * animal_data$sd_dailywt
  
  filtered_animal_data <- subset(animal_data, dailywt >= lower_limit & dailywt <= upper_limit)
  dailywts_out <- rbind(dailywts_out, filtered_animal_data)
}


# dailywts_out <- data.frame(RFID = character(), Date = character(), dailywt = numeric(), stringsAsFactors = FALSE)
# 
# staticwt_avg <- staticwts_selected %>%
#   group_by(RFID) %>%
#   summarise(avg_staticwt = round(mean(staticwt, na.rm = TRUE), 2))
# 
# for (i in unique(dailywts_filtered$RFID)) {
#   animal_data <- subset(dailywts_filtered, RFID == i)
#   
#   ref_data <- staticwt_avg %>%
#     filter(RFID == i)
#   
#   lower_limit <- ref_data$avg_staticwt /2
#   upper_limit <- ref_data$avg_staticwt * 1.5
#   
#   filtered_animal_data <- subset(animal_data, dailywt >= lower_limit & dailywt <= upper_limit)
#   dailywts_out <- rbind(dailywts_out, filtered_animal_data)
# }


# averaging of WOWs if more than one weight recorded in a single day
avg_dailywts_selected <- dailywts_out %>%
  group_by(RFID, Date) %>%
  summarise(avg_dailywt = round(mean(dailywt), 2))

#avg_dailywts <- dailywts_avg %>%
  #filter(n_distinct(Date) >= 2)

# linear interpolation to get the wows for missing days
dates_dailywt <- seq(as.Date("2023-02-16"), as.Date("2023-04-15"), by = "day")
new_dailywts <- expand.grid(RFID = unique(avg_dailywts$RFID), Date = dates_dailywt)

merged_dailywts <- merge(new_dailywts, avg_dailywts, by = c("RFID", "Date"), all.x = TRUE)

dailywts_splited <- split(merged_dailywts, f = merged_dailywts$RFID)

est_dailywts_list <- list()

for (i in seq_along(dailywts_splited)) {
  est_dailywts_list[[i]] <- with(dailywts_splited[[i]], approx(x = Date, y = avg_dailywt, xout = dates_dailywt)$y)
}

# getting 1DW
# avg_dailywts_selected <- cbind(merged_dailywts, avg_1DW = unlist(est_dailywts_list))
# avg_dailywts_selected$avg_1DW <- round(avg_dailywts_selected$avg_1DW, 2)


avg_dailywts_selected <- avg_dailywts
avg_dailywts_selected$avg_1DW <- round(avg_dailywts_selected$avg_dailywt, 2)


# getting rolling averages for window sizes 2 to 100
library(zoo)

window_sizes <- 2:30

rollavg_list <- list()

for (i in unique(avg_dailywts_selected$RFID)) {
  animal_data <- subset(avg_dailywts_selected, RFID == i)
  
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
  
  rollavg_list[[i]] <- temp_df
}

allwts_rollavg <- do.call(rbind, rollavg_list)

# selecting allwts_rollavg for the period of 29 days for which static weights are available
rollavg_selected <- allwts_rollavg[allwts_rollavg$Date >= "2023-03-03" & allwts_rollavg$Date <= "2023-03-31", ]

# merging static weights with rolling averag dataset to for further analysis
allwts_selected <- merge(rollavg_selected, staticwts_selected, by = c("RFID", "Date"))

# removing data points with NA values on est_staticwt
#allwts_selected <- subset(allwts_selected, !is.na(allwts_selected$est_staticwts))


# calculating concordance correlation coefficient for all simulated datasets
library(DescTools)

simulated_data <- allwts_selected[, !(names(allwts_selected) %in% c("RFID", "Date", "avg_dailywt", "staticwt", "est_staticwts", "stationname"))]


CCC_values <- data.frame(window = numeric(), ccc = numeric(), p_value = numeric(), RMSE = numeric(), rsq = numeric())

for (col_name in names(simulated_data)) {
  ccc_result <- CCC(simulated_data[[col_name]], allwts_selected$est_staticwts, ci = "z-transform", conf.level = 0.95, na.rm = TRUE)
  p_value <- t.test(simulated_data[[col_name]], allwts_selected$est_staticwts)$p.value
  
  squared_diff <- (simulated_data[[col_name]] - allwts_selected$est_staticwts)^2
  RMSE <- sqrt(mean(squared_diff, na.rm = TRUE))
  
  data <- data.frame(simulated_data[[col_name]], allwts_selected$est_staticwts)
  model <- lm(allwts_selected$est_staticwts ~ simulated_data[[col_name]], data = data)
  rsq <- summary(model)$r.squared
  
  CCC_values <- rbind(CCC_values, data.frame(window = col_name, ccc = ccc_result$rho.c, p_value = p_value, RMSE = RMSE, rsq = rsq))
}

CCC_values <- data.frame(window_size = 1:30, ccc = CCC_values$ccc.est, p_value = CCC_values$p_value, RMSE = CCC_values$RMSE, R_squared = CCC_values$rsq)


plot1 <- ggcptplot(CCC_values$ccc, cp_method = "AMOC") + xlab("Window size") + ylab("CCC value")

plot2 <- ggcptplot(CCC_values$RMSE, cp_method = "AMOC") + xlab("Window size") + ylab("RMSE value")

grid.arrange(plot1, plot2, nrow = 1)


plot1 <- ggplot(CCC_values, aes(x = window_size, y = ccc)) +
  geom_line() + geom_point() + xlab("Window size") + ylab("CCC")

plot2 <- ggplot(CCC_values, aes(x = window_size, y = RMSE)) +
  geom_line() + geom_point() + xlab("Window size")

grid.arrange(plot1, plot2, nrow = 1)


# t.test(allwts_selected$avg_1DW, allwts_selected$avg_2DW, paired = TRUE)$p.value
# 
# t.test(allwts_selected$avg_5DW, allwts_selected$avg_6DW, paired = TRUE)$p.value
# 
# t.test(allwts_selected$avg_6DW, allwts_selected$avg_7DW, paired = TRUE)$p.value
# 
# t.test(allwts_selected$avg_8DW, allwts_selected$avg_9DW, paired = TRUE)$p.value
# 
# t.test(allwts_selected$avg_9DW, allwts_selected$avg_10DW, paired = TRUE)$p.value



# plot1 <-ggplot() +
#   geom_point(data = CCC_values, aes(x = window_size, y = ccc)) +
#   geom_line(data = CCC_values, aes(x = window_size, y = R_squared))+
#   geom_point(data = CCC_values, aes(x = window_size, y = R_squared)) +
#   geom_line(data = CCC_values, aes(x = window_size, y = ccc))+
#   scale_x_continuous(breaks = seq(0, 30, 5), labels = seq(0, 30, 5))
# 
# 
# plot2 <- ggplot() +
#   geom_point(data = CCC_values, aes(x = window_size, y = RMSE)) +
#   geom_line(data = CCC_values, aes(x = window_size, y = RMSE)) +
#   scale_x_continuous(breaks = seq(0, 30, 5), labels = seq(0, 30, 5)) +
#   scale_y_continuous(sec.axis = sec.axis(~.*10, name = "RMSE"))
# 
# grid.arrange(plot1, plot2, ncol = 1)
# plot_grid(plot1, plot2, nrow = 1, align = "v")
# 
# 
# ggplot(CCC_values, aes(x = window_size)) +
#   geom_line(aes(y = ccc), color = "blue", linetype = "solid", size = 1) +
#   geom_line(aes(y = RMSE / 11), color = "red", linetype = "dashed", size = 1) +
#   scale_y_continuous(
#     breaks = seq(0.9, 1, 0.01),
#     name = "CCC values",
#     sec.axis = sec_axis(~ . * 11, name = "RMSE values"),
#   ) +
#   xlab("Window Size") +
#   ggtitle("Combined Plot")










