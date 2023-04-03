library(DMMongoDB)
library(dplyr)
library(tidyverse)

# saving data locally and in global environment
all_properties <- get_stations()
write.csv(all_properties, file = "Data/all_properties.csv")

### CQIRP animal data ####
CQIRP_animals_current <- get_cattle(property = "CQIRP")

CQIRP_dailywts_current <- get_dailywts(RFID = CQIRP_animals_current$RFID)

CQIRP_dailywts_all <- get_dailywts(property = "CQIRP")
write.csv(CQIRP_dailywts_all, file = "WOW analysis/CQIRP_dailywts_all.csv")

CQIRP_stwts_all <- get_staticwts(property = "CQIRP")
write.csv(CQIRP_stwts_all, file = "WOW analysis/CQIRP_stwts_all.csv")

CQIRP_animals_all <- get_cattle(RFID = unique(CQIRP_dailywts_all$RFID))
write.csv(CQIRP_animals_all, file = "WOW analysis/CQIRP_animals_all.csv")


#static weights ####
subset_stwts <- CQIRP_stwts_all[CQIRP_stwts_all$Weight != 0, ]

stwts_counts <- c()

for (i in subset_stwts$RFID) {
  stwts_counts[i] <- sum(subset_stwts$RFID == i)
}

df_stwts <- data.frame(RFID = c(unique(subset_stwts$RFID)),
                 num_stwts = stwts_counts)

# summary statistics
summary(subset_stwts$Weight)
sd(subset_stwts$Weight)

# plots
ggplot(df_stwts, aes(x = num_stwts, color = RFID)) +
  geom_bar()

ggplot(subset_stwts, aes(x = Date, y = Weight, color = RFID)) +
  geom_line()

ggplot(subset_stwts, aes(x = RFID)) +
  geom_bar()


#### Walk-over weights ####
subset_wow <- CQIRP_dailywts_all[CQIRP_dailywts_all$Weight != 0, ]
length(unique(subset_wow$RFID))

wow_counts <- c()
for(i in subset_wow$RFID) {
  wow_counts[i] <- sum(subset_wow$RFID == i)
}

df_wow <- data.frame(RFID = c(unique(subset_wow$RFID)),
                       num_wow = wow_counts)

sum(df_wow$num_wow >= 1 & df_wow$num_wow <= 10)
sum(df_wow$num_wow >= 11 & df_wow$num_wow <= 100)
sum(df_wow$num_wow >= 101 & df_wow$num_wow <= 1000)
sum(df_wow$num_wow >= 1001)

summary(subset_wow$Weight)
sd(subset_wow$Weight)

ggplot(df_wow, aes(x = num_wow)) +
  geom_histogram()


# Selecting animals with both static weights and wow records ####
RFIDsearch(RFID = subset_stwts$RFID)

wow_selected <- get_dailywts(RFID = subset_stwts$RFID)
wow_selected_rm0 <- subset(wow_selected[wow_selected$Weight != 0, ])

ggplot(wow_selected_rm0, aes(x = Date, y = Weight, color = RFID)) +
  geom_line()




















