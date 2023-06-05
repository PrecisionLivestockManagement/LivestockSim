library(DMMongoDB)
library(tidyverse)


all_properties <- get_stations()
write.csv(all_properties, file = "Data/all_properties_csv")


# number of currnet animals for each property
animals_current <- c()
for (i in all_properties$Stationname) {
  animals_current[i] <- c(length(get_cattle(property = i)$RFID))
  
}

animals_current

# number of all animals for each property #####
animals_all <- c()
for (i in all_properties$Stationname) {
  animals_all[i] <- c(length(unique(get_dailywts(property = i)$RFID)))
}

animals_all

# number of daily weights for each property ####
daily_wts <- c()

for (i in all_properties$Stationname) {
  daily_wts[i] <- length(get_dailywts(property = i)$Weight)
}

daily_wts

# counting non zero daily weights
dailywts <- c()
for (i in all_properties$Stationname) {
  dw <- get_dailywts(property = i)
  dw0 <- dw[dw$Weight > 0, ]
  dailywts[i] <- length(dw0$Weight)
}
dailywts

# minimum daily wts for all stations
min_dw <- c()
for (i in all_properties$Stationname) {
  dw <- get_dailywts(property = i)
  dw0 <- dw[dw$Weight > 0, ]
  min_dw[i] <- min(dw0$Weight)
}
min_dw

# maximum daily wts for all stations
max_dw <- c()
for (i in all_properties$Stationname) {
  dw <- get_dailywts(property = i)
  dw0 <- dw[dw$Weight > 0, ]
  max_dw[i] <- max(dw0$Weight)
}
max_dw

# average daily wts for all stations
avg_dw <- c()
for (i in all_properties$Stationname) {
  dw <- get_dailywts(property = i)
  dw0 <- dw[dw$Weight > 0, ]
  avg_dw[i] <- mean(dw0$Weight)
}
avg_dw

# standard deviation of daily wts for all stations
sd_dw <- c()
for (i in all_properties$Stationname) {
  dw <- get_dailywts(property = i)
  dw0 <- dw[dw$Weight > 0, ]
  sd_dw[i] <- sd(dw0$Weight)
}
sd_dw



# number of static weights for each property #####
static_wts <- c()
for (i in all_properties$Stationname) {
  static_wts[i] <- length(get_staticwts(property = i)$Weight)
}

static_wts

# counting non zero static weights
staticwts <- c()
for (i in all_properties$Stationname) {
  sw <- get_staticwts(property = i)
  sw0 <- sw[sw$Weight > 0, ]
  staticwts[i] <- length(sw0$Weight)
}
staticwts

# minimum static wts for all stations
min_sw <- c()
for (i in all_properties$Stationname) {
  sw <- get_staticwts(property = i)
  sw0 <- sw[sw$Weight > 0, ]
  min_sw[i] <- min(sw0$Weight)
}
min_sw

# maximum static wts for all stations
max_sw <- c()
for (i in all_properties$Stationname) {
  sw <- get_staticwts(property = i)
  sw0 <- sw[sw$Weight > 0, ]
  max_sw[i] <- max(sw0$Weight)
}
max_sw

# average static wts for all stations
avg_sw <- c()
for (i in all_properties$Stationname) {
  sw <- get_staticwts(property = i)
  sw0 <- sw[sw$Weight > 0, ]
  avg_sw[i] <- mean(sw0$Weight)
}
avg_sw

# standard deviation of static wts for all stations
sd_sw <- c()
for (i in all_properties$Stationname) {
  sw <- get_staticwts(property = i)
  sw0 <- sw[sw$Weight > 0, ]
  sd_sw[i] <- sd(sw0$Weight)
}
sd_sw


all_data <- cbind(all_properties, animals_current, animals_all, 
                             daily_wts, dailywts, min_dw, max_dw, avg_dw, sd_dw, 
                             static_wts, staticwts, min_sw, max_sw, avg_sw, sd_sw)

# removing properties with no daily weight records
all_properties_data <- all_data[all_data$dailywts != 0, ]


## plotting the data ####
# number of animals in each property
df_long1 <- all_properties_data %>% pivot_longer(cols = c(animals_current, animals_all), names_to = "variable", values_to = "value")

ggplot(df_long1, aes(x = Stationname, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "No. of animals in each property",
       x = "Property Names",
       y = "No. of animals",
       fill = "Variable")

# number of daily weight and static weight records for each property
df_long2 <- all_properties_data %>% pivot_longer(cols = c(dailywts, staticwts), names_to = "variable", values_to = "value")

ggplot(df_long2, aes(x = Stationname, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "No. of weight records in each property",
       x = "Property Names",
       y = "No. of records",
       fill = "Variable")


# average daily wts and static wts
df_long3 <- all_properties_data %>% pivot_longer(cols = c(avg_dw, avg_sw), names_to = "variable", values_to = "value")

ggplot(df_long3, aes(x = Stationname, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = value - ifelse(variable == "avg_dw", sd_dw, sd_sw),
                    ymax = value + ifelse(variable == "avg_dw", sd_dw, sd_sw)),
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Average daily wts and static wts for properties with at least one daily wt record",
       x = "Property Names",
       y = "Average Weight",
       fill = "Variable") +
  scale_fill_manual(values = c("avg_dw" = "lightblue", "avg_sw" = "pink")) +
  theme_minimal()


properties_all <- c("Killara", "Belmont", "Gadara", "Gyranda", "CQIRP", "Berrigurra", "Anguil", "Belcarce", "Karalee Plains", "EL Clarín", "El Jaguel", "San Mauricio", "El Futuro", "La Marina", "Don Enrique", "Pohitahue", "La Aguada", "La Esmeralda", "La Preferencia", "Carisma", "El Trabajo", "La Aurora", "EEA Cesáreo Naredo", "El Bagual", "El Palenque", "La Emilia", "Mathison", "Clonagh", "La Florida", "Netley", "Glenmoan", "Damper", "La Fermina", "La Tijereta", "Corfield Downs", "Crediton", "Tremere", "Valle Inferior")
properties_dw <- c("Killara", "Belmont", "Gyranda", "CQIRP", "Berrigurra", "Anguil", "Belcarce", "El Jaguel", "La Esmeralda", "Carisma", "La Emilia", "Mathison", "La Florida", "Glenmoan", "La Fermina", "La Tijereta", "Corfield Downs", "Tremere", "Valle Inferior")
properties_sw <- c("Killara", "Belmont", "Gyranda", "CQIRP", "Berrigurra", "Anguil", "Belcarce", "Carisma", "Mathison", "Tremere")


all_staticwts <- rbind(Killara_staticwts, Belmont_staticwts, Gyranda_staticwts, CQIRP_staticwts, Berrigurra_staticwts, Anguil_staticwts, Belcarce_staticwts, Carisma_staticwts, Mathison_staticwts, Tremere_staticwts)
all_dailywts <- rbind(Killara_dailywts, Belmont_dailywts, Gyranda_dailywts, CQIRP_dailywts, Berrigurra_dailywts, Anguil_dailywts, Belcarce_dailywts, ElJaguel_dailywts, LaEsmeralda_dailywts, Carisma_dailywts, LaEmilia_dailywts, Mathison_dailywts, LaFlorida_dailywts, Glenmoan_dailywts, LaFermina_dailywts, LaTijereta_dailywts, CorfieldDowns_dailywts, Tremere_dailywts, ValleInferior_dailywts)

all_staticwts[which(all_staticwts$Date == "0014-06-20 00:12:08"), "Date"] <- "2014-06-20 00:12:08"
all_staticwts <- all_staticwts[all_staticwts$Date != "2010-07-20 00:00:00", ]

ggplot(all_staticwts, mapping = aes(Date)) +
  geom_histogram() +
  facet_wrap(~ stationname) +
  labs(title = "No. of static weights collected over time")

ggplot(all_dailywts, mapping = aes(Date)) +
  geom_histogram() +
  facet_wrap(~ stationname)










#####################################################

Gyranda_dailywts_ <- get_dailywts(property = "Gyranda")
Gyranda_animals <- get_cattle(RFID = unique(Gyranda_dailywts_$RFID))
Gyranda_dailywts_0 <- Gyranda_dailywts_[Gyranda_dailywts_$Weight > 0, ]



ggplot(Gyranda_dailywts_0, aes(x = Date, y = Weight, color = RFID)) +
  geom_line()+
  guides(color = "none")























