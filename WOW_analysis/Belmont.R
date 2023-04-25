library(DMMongoDB)
library(tidyverse)
library(zoo)
library(scales)

############################## Accuracy measurement ################################

Belmont_animals <- get_cattle(property = "Belmont")


## gauthering static weights
Belmont_staticwts_ <- get_staticwts(RFID = Belmont_animals$RFID)

ggplot(Belmont_staticwts_, aes(x = Date, y = Weight, color = RFID)) +
  geom_point() +
  guides(color = "none")

# selecting date range from 2023-02-09 to 2023-03-31 (Date with most static weights for each animal)
Belmont_staticwts_ <- Belmont_staticwts_[Belmont_staticwts_$Date >= "2023-02-09" & Belmont_staticwts_$Date <= "2023-03-31", ]

ggplot(Belmont_staticwts, aes(x = Date, y = Weight, color = RFID)) +
  geom_point() +
  guides(color = "none")

# selecting animals with more than 1 static weight measures
stwts_counts <- c()

for (i in Belmont_staticwts_$RFID) {
  stwts_counts[i] <- sum(Belmont_staticwts_$RFID == i)
}

df_stwts_counts_ <- data.frame(RFID = c(unique(Belmont_staticwts_$RFID)),
                       num_stwts = stwts_counts)

df_stwts_counts <- df_stwts_counts_[df_stwts_counts_$num_stwts > 1, ]

# getting static weights for animals with more than one static weight records
Belmont_staticwts <- get_staticwts(RFID = df_stwts_counts$RFID)
Belmont_staticwts <- Belmont_staticwts[Belmont_staticwts$Weight > 0, ]

# linear interpolation to get everyday static weights for all animals
library(lubridate)

all_dates <- seq(min(Belmont_staticwts$Date), max(Belmont_staticwts$Date), by = "day")
new_staticwts <- expand.grid(RFID = unique(Belmont_staticwts$RFID), Date = all_dates)

merged_staticwts <- merge(new_staticwts, Belmont_staticwts, by = c("RFID", "Date"), all.x = TRUE, all.y = TRUE)

staticwts_splited <- split(merged_staticwts, f = merged_staticwts$RFID)


# estimating static weights for each animal each day
est_staticwts_list <- list()

for (i in seq_along(staticwts_splited)) {
  est_staticwts_list[[i]] <- with(staticwts_splited[[i]], approx(x = Date, y = Weight, xout = all_dates)$y)
}

est_staticwts <- cbind(merged_staticwts, est_staticwts = unlist(est_staticwts_list))

#removing n/a rows
est_staticwts <- est_staticwts[!is.na(est_staticwts$est_staticwts), ]

ggplot(est_staticwts, aes(x = Date, y = est_staticwts, color = RFID)) +
  geom_point() +
  guides(color = "none")





## gauthering daily weights 
Belmont_dailywts_ <- get_dailywts(RFID = df_stwts_counts$RFID)

# selecting date range 2023-02-09 to 2023-03-31
dailywts_raw <- Belmont_dailywts_[Belmont_dailywts_$Date >= "2023-02-09" & Belmont_dailywts_$Date <= "2023-03-31", ]


# averaging the daily weights for each day
dailywts_raw$DateTime <- dailywts_raw$Date
dailywts_raw$Date <- as.Date(dailywts_raw$Date)

avg_dailywts_raw <- dailywts_raw %>%
  group_by(RFID, Date) %>%
  summarise(avg_dailywts = mean(Weight))



## Raw data analysis
allwts_raw <- merge(avg_dailywts_raw, est_staticwts, by = c("RFID", "Date"))


ggplot(allwts_raw, aes(x = est_staticwts, y = avg_dailywts)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Static wts vs daily wts for Belmont animals")

# differences between static weights and wow
allwts_raw$diff <- c(allwts_raw$avg_dailywts - allwts_raw$est_staticwts)
allwts_raw$diff_percent <- abs(allwts_raw$diff)/allwts_raw$est_staticwts * 100

ggplot(allwts_raw, aes(diff)) +
  geom_histogram(color = "black") +
  scale_y_continuous(labels = function(x) percent(x / 2307)) +
  labs(title = "Difference of daily weight form static weights", x = "average daily weight minus static weight(kg)", y = "Percentage of weights")

# calculating concordance correlation coefficient to assess degree of agreement between static weights and daily wts
library(DescTools)

ccc_result <-CCC(allwts_raw$avg_dailywts, allwts_raw$est_staticwts)
ccc_value <- ccc_result[["rho.c"]]


## Outlier removed data analysis
dailywts_out <- dailywts_raw[dailywts_raw$Weight > 40 & dailywts_raw$Weight < 600, ]

avg_dailywts_out <- dailywts_out %>%
  group_by(RFID, Date) %>%
  summarise(avg_dailywts = mean(Weight))




allwts_out <- merge(avg_dailywts_out, est_staticwts, by = c("RFID", "Date"))


ggplot(allwts_out, aes(x = est_staticwts, y = avg_dailywts)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Static wts vs daily wts for Belmont animals")

# differences between static weights and wow
allwts_out$diff <- c(allwts_out$avg_dailywts - allwts_out$est_staticwts)
allwts_out$diff_percent <- abs(allwts_out$diff)/allwts_out$est_staticwts * 100

ggplot(allwts_out, aes(diff)) +
  geom_histogram(color = "black") +
  scale_y_continuous(labels = function(x) percent(x / 2307)) +
  labs(title = "Difference of daily weight form static weights", x = "average daily weight minus static weight(kg)", y = "Percentage of weights")

# calculating concordance correlation coefficient to assess degree of agreement between static weights and daily wts
library(DescTools)

ccc_result <-CCC(allwts_out$avg_dailywts, allwts_out$est_staticwts)
ccc_value <- ccc_result[["rho.c"]]


## smoothed data analysis (using 5 days rolling average)
dailywts_splitted <- split(allwts_out, f = allwts_out$RFID)

smooth_dailywts_list <- list()

for (i in seq_along(dailywts_splitted)) {
  smooth_dailywts_list[[i]] <- rollmean(dailywts_splitted[[i]]$avg_dailywts, k = 5, align = 'center', na.pad = TRUE)
}

allwts_smooth <- cbind(allwts_out, smooth_dailywts = unlist(smooth_dailywts_list))

# removing n/a values as a result of rolling averages of 5 days
allwts_smooth <- allwts_smooth[!is.na(allwts_smooth$smooth_dailywts), ]

ggplot(allwts_smooth, aes(x = est_staticwts, y = smooth_dailywts)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Static wts vs daily wts for Belmont animals")

ccc_result <-CCC(allwts_smooth$smooth_dailywts, allwts_smooth$est_staticwts)
ccc_value <- ccc_result[["rho.c"]]

allwts_smooth$diff <- c(allwts_smooth$smooth_dailywts - allwts_smooth$est_staticwts)
allwts_smooth$diff_percent <- abs(allwts_smooth$diff)/allwts_smooth$est_staticwts * 100

ggplot(allwts_smooth, aes(diff)) +
  geom_histogram(color = "black") +
  scale_y_continuous(labels = function(x) percent(x / 2307)) +
  labs(title = "Difference of daily weight form static weights", x = "average daily weight minus static weight(kg)", y = "Percentage of weights")



############################ Repeatability measurement ###############################
library(lme4)

## raw data
model_raw <- lmer(Weight ~ 1 + (1 | RFID), data = dailywts_raw)

# Calculate the estimated variance components using the REML model
vc_raw <- VarCorr(model_raw)
VarA_raw <- vc_raw[["RFID"]][[1]]  # variance component between animals
VarR_raw <- attr(vc_raw, "sc")^2  # residual or random variance

# Calculate the repeatability
repeatability_raw <- VarA_raw / (VarR_raw + VarR_raw)
repeatability_raw


## outlier removed data
model_out <- lmer(Weight ~ 1 + (1 | RFID), data = dailywts_out)

# Calculate the estimated variance components using the REML model
vc_out <- VarCorr(model_out)
VarA_out <- vc_out[["RFID"]][[1]]  # variance component between animals
VarR_out <- attr(vc_out, "sc")^2  # residual or random variance

# Calculate the repeatability
repeatability_out <- VarA_out / (VarR_out + VarR_out)
repeatability_out





  





