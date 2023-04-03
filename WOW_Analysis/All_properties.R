library(DMMongoDB)
library(dplyr)
library(tidyverse)

all_properties <- get_stations()
write.csv(all_properties, file = "Data/all_properties_csv")


# number of currnet animals for each property
animals_current <- c()
for (i in all_properties$Stationname) {
  animals_current[i] <- c(length(get_cattle(property = i)$RFID))
  
}

animals_current

# number of all animals for each property
animals_all <- c()
for (i in all_properties$Stationname) {
  animals_all[i] <- c(length(unique(get_dailywts(property = i)$RFID)))
}

animals_all

# number of daily weights for each property
daily_wts <- c()
for (i in all_properties$Stationname) {
  daily_wts[i] <- length(get_dailywts(property = i)$Weight)
}

daily_wts

# number of static weights for each property
static_wts <- c()
for (i in all_properties$Stationname) {
  static_wts[i] <- length(get_staticwts(property = i)$Weight)
}

static_wts

