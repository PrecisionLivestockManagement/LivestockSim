library(DMMongoDB)
library(tidyverse)
library(lubridate)

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
avg_dailywts <- dailywts_out %>%
  group_by(RFID, Date) %>%
  summarise(avg_dailywt = round(mean(dailywt), 2))


# model development ####
# selecting weight records between 2023-02-09 to 2023-03-31, 
# with static weight recorded at least at the start and end of this period
staticwts_model <- staticwts_raw[staticwts_raw$Date >= "2023-02-09" & staticwts_raw$Date <= "2023-03-31", ]

staticwts_filtered <- staticwts_raw %>%
  group_by(RFID) %>%
  filter(any(Date == as.Date("2023-02-09")) & any(Date == as.Date("2023-03-31")))

RFID_filtered <- c(unique(staticwts_filtered$RFID))


# selecting wow between 2022-12-21 to 2023-05-20 (50 day prior and 50 days after of static weight days) for animals selected at staticwts_filtered
dailywts_model <- avg_dailywts[avg_dailywts$Date >= "2022-12-21" & avg_dailywts$Date <= "2023-05-20", ]
dailywts_filtered <- dailywts_model %>%
  filter(RFID %in% RFID_filtered)

# RFID = 982 123766108788, do not have any wow recorded at the 51 day period, 
# RFID = 982 123766109224 do not have wow recorded form 2023-02-09 to 2023-03-08;  hence it is not included
# therefore animals selected will be n = 21 not 23
# removing RFID = 982 123766108788 from staticwts_filtered and dailywts_filtered
staticwts_filtered <- staticwts_filtered[staticwts_filtered$RFID != "982 123766108788" & staticwts_filtered$RFID != "982 123766109224", ]
dailywts_filtered <- dailywts_filtered[dailywts_filtered$RFID != "982 123766108788" & dailywts_filtered$RFID != "982 123766109224", ]


# linear interpolation to get the static weights for the missing days
dates_staticwt <- seq(as.Date("2023-02-09"), as.Date("2023-03-31"), by = "day")
new_staticwts <- expand.grid(RFID = unique(staticwts_filtered$RFID), Date = dates_staticwt)

merged_staticwts <- merge(new_staticwts, staticwts_filtered, by = c("RFID", "Date"), all.x = TRUE)

staticwts_splited <- split(merged_staticwts, f = merged_staticwts$RFID)

est_staticwts_list <- list()

for (i in seq_along(staticwts_splited)) {
  est_staticwts_list[[i]] <- with(staticwts_splited[[i]], approx(x = Date, y = staticwt, xout = dates_staticwt)$y)
}

staticwts_selected <- cbind(merged_staticwts, est_staticwts = unlist(est_staticwts_list))
staticwts_selected <- staticwts_selected[, -which(names(staticwts_selected) == "stationname")]
staticwts_selected$est_staticwts <- round(staticwts_selected$est_staticwts, 2)

# linear interpolation to get the wows for missing days
dates_dailywt <- seq(as.Date("2022-12-21"), as.Date("2023-05-20"), by = "day")
new_dailywts <- expand.grid(RFID = unique(dailywts_filtered$RFID), Date = dates_dailywt)

merged_dailywts <- merge(new_dailywts, dailywts_filtered, by = c("RFID", "Date"), all.x = TRUE)

dailywts_splited <- split(merged_dailywts, f = merged_dailywts$RFID)

est_dailywts_list <- list()

for (i in seq_along(dailywts_splited)) {
  est_dailywts_list[[i]] <- with(dailywts_splited[[i]], approx(x = Date, y = avg_dailywt, xout = dates_dailywt)$y)
}

# getting 1DW
avg_dailywts_selected <- cbind(merged_dailywts, avg_1DW = unlist(est_dailywts_list))
avg_dailywts_selected$avg_1DW <- round(avg_dailywts_selected$avg_1DW, 2)

# avg_dailywts_splitted <- split(avg_dailywts_selected, f = avg_dailywts_selected$RFID)

# getting rolling averages for window sizes 2 to 100
library(zoo)

window_sizes <- 2:100

rollavg_list <- list()

for (i in unique(avg_dailywts_selected$RFID)) {
  animal_data <- subset(avg_dailywts_selected, RFID == i)
  
  rolling_averages <- lapply(window_sizes, function(k) {
    rollmean(animal_data$avg_1DW, k = k, align = 'center', na.pad = TRUE)
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

# selecting allwts_rollavg for the period of 51 days for which static weights are available
rollavg_selected <- allwts_rollavg[allwts_rollavg$Date >= "2023-02-09" & allwts_rollavg$Date <= "2023-03-31", ]

# merging static weights with rolling averag dataset to for further analysis
allwts_selected <- merge(rollavg_selected, staticwts_selected, by = c("RFID", "Date"))


# calculating concordance correlation coefficient for all simulated datasets
library(DescTools)

simulated_data <- allwts_selected[, !(names(allwts_selected) %in% c("RFID", "Date", "avg_dailywt", "staticwt", "est_staticwts"))]

LW_CCC_values <- data.frame(window = numeric(), ccc = numeric(), p_value = numeric())

for (col_name in names(simulated_data)) {
  ccc_result <- CCC(simulated_data[[col_name]], allwts_selected$est_staticwts, ci = "z-transform", conf.level = 0.95, na.rm = TRUE)
  p_value <- t.test(simulated_data[[col_name]], allwts_selected$est_staticwts)$p.value
  
  LW_CCC_values <- rbind(LW_CCC_values, data.frame(window = col_name, ccc = ccc_result$rho.c, p_value = p_value))
}

LW_CCC_values <- data.frame(window_size = 1:100, ccc = LW_CCC_values$ccc.est, p_value = LW_CCC_values$p_value)

ggplot(LW_CCC_values, aes(x = window_size, y = ccc)) +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 100, 5), labels = seq(0, 100, 5))




# # simulation for LWC
# # calculating daily LWC using static weight data
# LWC_static <- staticwts_selected %>%
#   group_by(RFID) %>%
#   mutate(LWC_static = ifelse(row_number() == 1, NA, c(est_staticwts[1], diff(est_staticwts)))) %>%
#   ungroup()
# 
# # calculating daily LWC using the simulated data
# LWC_simulated_list <- list()
# 
# 
# for (i in unique(rollavg_selected$RFID)) {
#   subset_data <- subset(rollavg_selected, RFID == i)
#   
#   weight_changes <- lapply(1:100, function(k) {
#     avg_col <- paste0("avg_", k, "DW")
#     c(NA, diff(subset_data[, avg_col]))
#   })
#   
#   daily_changes <- data.frame(
#     RFID = subset_data$RFID,
#     Date = subset_data$Date,
#     weight_changes
#   )
#   
#   colnames(daily_changes)[-c(1, 2)] <- paste0("LWC_", 1:10, "DW")
#   
#   LWC_simulated_list[[i]] <- daily_changes
# }
# 
# LWC_simulated <- do.call(rbind, LWC_simulated_list)
# 
# 
# # calculating concordance correlation coefficient for all LWC simulated datasets
# library(DescTools)
# 
# LWC_sim <- LWC_simulated[, !(names(LWC_simulated) %in% c("RFID", "Date", "avg_dailywt", "staticwt", "est_staticwts"))]
# 
# LWC_CCC_values <- data.frame(window = numeric(), ccc = numeric(), p_value = numeric())
# 
# for (col_name in names(LWC_sim)) {
#   ccc_result <- CCC(LWC_sim[[col_name]], LWC_static$LWC_static, ci = "z-transform", conf.level = 0.95, na.rm = TRUE)
#   p_value <- t.test(LWC_sim[[col_name]], LWC_static$LWC_static)$p.value
#   
#   LWC_CCC_values <- rbind(values, data.frame(window = col_name, ccc = ccc_result$rho.c, p_value = p_value))
# }
# 
# LWC_CCC_values <- data.frame(window = 1:100, ccc = values$ccc.est, p_value = values$p_value)
# 
# ggplot(LWC_CCC_values, aes(x = window, y = ccc)) +
#   geom_point() +
#   scale_x_continuous(breaks = seq(0, 100, 5), labels = seq(0, 100, 5))










