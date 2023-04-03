library(DMMongoDB)
library(dplyr)
library(zoo)
library(tidyverse)

############ getting all cattle at Belmont ##################
Bel_animals_current <- get_cattle(property = "Belmont")
Bel_dailywts_current <- get_dailywts(Bel_animals_current)

Bel_dailywts_all <- get_dailywts(property = "Belmont")
Bel_animals_all <- get_cattle(RFID = Bel_dailywts_all$RFID)

Bel_animals_growing <- subset(Bel_animals_all[Bel_animals_all$category == "growing", ])

Bel_dailywts_growing <- get_dailywts(RFID = Bel_animals_growing)
Bel_stwts_growing <- get_staticwts(RFID = Bel_animals_growing)


####### outlier removal ####
#Removing zero weights
dailywts_growing <- Bel_dailywts_growing[Bel_dailywts_growing$Weight != 0, ]
stwts_growing <- Bel_stwts_growing[Bel_stwts_growing$Weight != 0, ]

# removing incorrect date
stwts_growing <- stwts_growing[stwts_growing$Date != "0014-06-20 00:12:08", ]

#### plotting ####
ggplot(stwts_growing, aes(x = Date, y = Weight)) +
  geom_point()

ggplot(dailywts_growing, aes(x = Date, y = Weight)) +
  geom_point()

















