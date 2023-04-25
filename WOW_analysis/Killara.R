library(DMMongoDB)
library(tidyverse)

## Getting animal data #####
Killara_animals_current <- get_cattle(property = "Killara")

Killara_daily_wts <- get_dailywts(property = "Killara") #raw data
Killara_daily_wts <- Killara_daily_wts[Killara_daily_wts$Weight > 0, ]
Killara_daily_wts$DateTime <- Killara_daily_wts$Date
Killara_daily_wts$Date <- as.Date(Killara_daily_wts$Date)

Killara_static_wts <- get_staticwts(property = "Killara")

Killara_all_wts <- merge(Killara_daily_wts, Killara_static_wts, by = c("RFID", "Date"), all.x = TRUE, all.y = TRUE)
Killara_all_wts <- Killara_all_wts[!is.na(Killara_all_wts$Weight.y), ]


ggplot(Killara_static_wts, aes(x = Date, y = Weight, color = RFID)) +
  geom_point() +
  guides(color = "none")

ggplot(Killara_daily_wts, aes(x = Date, y = Weight, color = RFID)) +
  geom_line() +
  guides(color = "none")

Belmont_daily_wts <- get_dailywts(property = "Belmont")
ggplot(Belmont_daily_wts, aes(x = Date, y = Weight, color = RFID)) +
  geom_point() +
  guides(color = "none")

Belmont_static_wts <- get_staticwts(property = "Belmont")
ggplot(Belmont_static_wts, aes(x = Date, y = Weight, color = RFID)) +
  geom_point() +
  guides(color = "none")

Gyranda_daily_wts <- get_dailywts(property = "Gyranda")
ggplot(Gyranda_daily_wts, aes(x = Date, y = Weight, color = RFID)) +
  geom_point() +
  guides(color = "none")

Gyranda_static_wts <- get_staticwts(property = "Gyranda")
ggplot(Gyranda_static_wts, aes(x = Date, y = Weight, color = RFID)) +
  geom_point() +
  guides(color = "none")

CQIRP_daily_wts <- get_dailywts(property = "CQIRP")
ggplot(CQIRP_daily_wts, aes(x = Date, y = Weight, color = RFID)) +
  geom_point() +
  guides(color = "none")

CQIRP_static_wts <- get_staticwts(property = "CQIRP")
ggplot(CQIRP_static_wts, aes(x = Date, y = Weight, color = RFID)) +
  geom_point() +
  guides(color = "none")

Berrigurra_daily_wts <- get_dailywts(property = "Berrigurra")
ggplot(Berrigurra_daily_wts, aes(x = Date, y = Weight, color = RFID)) +
  geom_point() +
  guides(color = "none")

Berrigurra_static_wts <- get_staticwts(property = "Berrigurra")
ggplot(Berrigurra_static_wts, aes(x = Date, y = Weight, color = RFID)) +
  geom_point() +
  guides(color = "none")

Carisma_daily_wts <- get_dailywts(property = "Carisma")
ggplot(Carisma_daily_wts, aes(x = Date, y = Weight, color = RFID)) +
  geom_point() +
  guides(color = "none")

Carisma_static_wts <- get_staticwts(property = "Carisma")
ggplot(Carisma_static_wts, aes(x = Date, y = Weight, color = RFID)) +
  geom_point() +
  guides(color = "none")

# Mathison_daily_wts <- get_dailywts(property = "Mathison")
# ggplot(Mathison_daily_wts, aes(x = Date, y = Weight, color = RFID)) +
#   geom_point() +
#   guides(color = "none")
# 
# Mathison_static_wts <- get_staticwts(property = "Mathison")
# ggplot(Mathison_static_wts, aes(x = Date, y = Weight, color = RFID)) +
#   geom_point() +
#   guides(color = "none")

Tremere_daily_wts <- get_dailywts(property = "Tremere")
ggplot(Tremere_daily_wts, aes(x = Date, y = Weight, color = RFID)) +
  geom_point() +
  guides(color = "none")

Tremere_static_wts <- get_staticwts(property = "Tremere")
ggplot(Tremere_static_wts, aes(x = Date, y = Weight, color = RFID)) +
  geom_point() +
  guides(color = "none")

Anguil_daily_wts <- get_dailywts(property = "Anguil")
ggplot(Anguil_daily_wts, aes(x = Date, y = Weight, color = RFID)) +
  geom_point() +
  guides(color = "none")

Anguil_static_wts <- get_staticwts(property = "Anguil")
ggplot(Anguil_static_wts, aes(x = Date, y = Weight, color = RFID)) +
  geom_point() +
  guides(color = "none")

Belcarce_daily_wts <- get_dailywts(property = "Belcarce")
ggplot(Belcarce_daily_wts, aes(x = Date, y = Weight, color = RFID)) +
  geom_point() +
  guides(color = "none")

Belcarce_static_wts <- get_staticwts(property = "Belcarce")
ggplot(Belcarce_static_wts, aes(x = Date, y = Weight, color = RFID)) +
  geom_line() +
  guides(color = "none")

