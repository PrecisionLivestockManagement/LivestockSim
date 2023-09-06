library(DMMongoDB)
library(tidyverse)

dailywts_alltime <- read.csv(file = "Data/Belmont_dailywts_alltime.csv")
staticwts_alltime <- read.csv(file = "Data/Belmont_staticwts_alltime.csv")


# changing some column names and removing zero weights
dailywts_alltime <- select(dailywts_all, -X)
colnames(dailywts_alltime)[colnames(dailywts_alltime) == "Date"] <- "DateTime"
dailywts_alltime$Date <- as.Date(dailywts_alltime$DateTime, format = "%d-%m-%y")
colnames(dailywts_alltime)[colnames(dailywts_alltime) == "Weight"] <- "dailywt"
dailywts_raw <- dailywts_alltime[dailywts_alltime$dailywt > 0, ]

staticwts_alltime <- select(staticwts_alltime, -X)
colnames(staticwts_alltime)[colnames(staticwts_alltime) == "Weight"] <- "staticwt"
staticwts_alltime$Date <- as.Date(staticwts_alltime$Date)
staticwts_raw <- staticwts_alltime[staticwts_alltime$staticwt > 0 & staticwts_alltime$Date != "0014-06-20", ]


# getting DOB

DOB <- get_cattle(property = "Belmont", fields = c("RFID", "properties.Management", "properties.birthWeight"))

DOB_alltime <- get_cattle(RFID = unique(dailywts_alltime$RFID), fields = c("RFID", "properties.Management", "properties.birthDate"))

DOB_all <- subset(DOB_alltime, birthDate != "1970-01-01")





ggplot(dailywts_herd1_raw, aes(x = Date, y = dailywt, color = RFID)) +
  geom_point() +
  guides(color = "none")

ggplot(dailywts_all, aes(x = as.Date(Date), y = Weight, color = RFID)) +
  geom_line() +
  scale_x_date(date_breaks = "1 year") +
  guides(color = "none")


dailywts_alltime_selected <- subset(dailywts_alltime, Date >= "2019-10-09")


dailywts_alltime <- get_dailywts(property = "Belmont")
animals <- get_cattle(property = "Belmont")
dailywts_all <- get_dailywts(RFID = unique(animals$RFID))

# changing some column names and removing zero weights
dailywts_alltime$DateTime <- dailywts_alltime$Date
dailywts_alltime$Date <- as.Date(dailywts_alltime$Date, format = "%d-%m-%y")

staticwts_alltime$Date <- as.Date(staticwts_alltime$Date)
staticwts_alltime <- staticwts_alltime[staticwts_alltime$Date != "0014-06-20", ]

colnames(dailywts_alltime)[colnames(dailywts_alltime) == "Weight"] <- "dailywt"
dailywts_raw <- dailywts_alltime[dailywts_alltime$dailywt > 0, ]

ggplot(dailywts_raw, aes(x = Date, y = dailywt, color = RFID)) +
  geom_point() +
  guides(color = "none")

alltime_2015 <- subset(dailywts_alltime, dailywts_alltime$Date >= "2015-01-01" & dailywts_alltime$Date <= "2015-12-31")
alltime_2016 <- subset(dailywts_alltime, dailywts_alltime$Date >= "2016-01-01" & dailywts_alltime$Date <= "2016-12-31")
alltime_2017 <- subset(dailywts_alltime, dailywts_alltime$Date >= "2017-01-01" & dailywts_alltime$Date <= "2017-12-31")
alltime_2018 <- subset(dailywts_alltime, dailywts_alltime$Date >= "2018-01-01" & dailywts_alltime$Date <= "2018-12-31")
alltime_2019 <- subset(dailywts_alltime, dailywts_alltime$Date >= "2019-01-01" & dailywts_alltime$Date <= "2019-12-31")
alltime_2020 <- subset(dailywts_alltime, dailywts_alltime$Date >= "2020-01-01" & dailywts_alltime$Date <= "2020-12-31")
alltime_2021 <- subset(dailywts_alltime, dailywts_alltime$Date >= "2021-01-01" & dailywts_alltime$Date <= "2021-12-31")
alltime_2022 <- subset(dailywts_alltime, dailywts_alltime$Date >= "2022-01-01" & dailywts_alltime$Date <= "2022-12-31")
alltime_2023 <- subset(dailywts_alltime, dailywts_alltime$Date >= "2023-01-01" & dailywts_alltime$Date <= "2023-12-31")


RFID_2015 <- c(unique(alltime_2015$RFID), rep(NA, 1181-109))
RFID_2016 <- c(unique(alltime_2016$RFID), rep(NA, 1181-121))
RFID_2017 <- c(unique(alltime_2017$RFID), rep(NA, 1181-284))
RFID_2018 <- c(unique(alltime_2018$RFID), rep(NA, 1181-584))
RFID_2019 <- c(unique(alltime_2019$RFID), rep(NA, 1181-821))
RFID_2020 <- c(unique(alltime_2020$RFID), rep(NA, 1181-960))
RFID_2021 <- c(unique(alltime_2021$RFID), rep(NA, 1181-1181))
RFID_2022 <- c(unique(alltime_2022$RFID), rep(NA, 1181-599))
RFID_2023 <- c(unique(alltime_2023$RFID), rep(NA, 1181-355))

RFID_1516 <- intersect(RFID_2015, RFID_2016)


RFID <- data.frame(RFID_2015, RFID_2016, RFID_2017, RFID_2018, RFID_2019, RFID_2020, RFID_2021, RFID_2022, RFID_2023)



dailywts_03Bel <- subset(dailywts_alltime, dailywts_alltime$Location == "03.Bel" & dailywts_alltime$dailywt > 0)
dailywts_04Bel <- subset(dailywts_alltime, dailywts_alltime$Location == "04.Bel"& dailywts_alltime$dailywt > 0)
dailywts_06Bel <- subset(dailywts_alltime, dailywts_alltime$Location == "06.Bel"& dailywts_alltime$dailywt > 0)
dailywts_07Bel <- subset(dailywts_alltime, dailywts_alltime$Location == "07.Bel"& dailywts_alltime$dailywt > 0)
dailywts_09Bel <- subset(dailywts_alltime, dailywts_alltime$Location == "09.Bel"& dailywts_alltime$dailywt > 0)
dailywts_12Bel <- subset(dailywts_alltime, dailywts_alltime$Location == "12.Bel"& dailywts_alltime$dailywt > 0)
dailywts_13Bel <- subset(dailywts_alltime, dailywts_alltime$Location == "13.Bel"& dailywts_alltime$dailywt > 0)
dailywts_15Bel <- subset(dailywts_alltime, dailywts_alltime$Location == "15.Bel"& dailywts_alltime$dailywt > 0)
dailywts_Bel17 <- subset(dailywts_alltime, dailywts_alltime$Location == "Bel17"& dailywts_alltime$dailywt > 0)
dailywts_Bel24 <- subset(dailywts_alltime, dailywts_alltime$Location == "Bel24"& dailywts_alltime$dailywt > 0)
dailywts_Bel2ab <- subset(dailywts_alltime, dailywts_alltime$Location == "Bel2ab"& dailywts_alltime$dailywt > 0)
dailywts_Bel33 <- subset(dailywts_alltime, dailywts_alltime$Location == "Bel33"& dailywts_alltime$dailywt > 0)
dailywts_Bel34 <- subset(dailywts_alltime, dailywts_alltime$Location == "Bel34"& dailywts_alltime$dailywt > 0)
dailywts_Bel3B <- subset(dailywts_alltime, dailywts_alltime$Location == "Bel3B"& dailywts_alltime$dailywt > 0)
dailywts_Bel40 <- subset(dailywts_alltime, dailywts_alltime$Location == "Bel40"& dailywts_alltime$dailywt > 0)
dailywts_Bel65 <- subset(dailywts_alltime, dailywts_alltime$Location == "Bel65"& dailywts_alltime$dailywt > 0)
dailywts_Bel_17 <- subset(dailywts_alltime, dailywts_alltime$Location == "Bel_17"& dailywts_alltime$dailywt > 0)
dailywts_Bel_19 <- subset(dailywts_alltime, dailywts_alltime$Location == "Bel_19"& dailywts_alltime$dailywt > 0)
dailywts_Bel_2A <- subset(dailywts_alltime, dailywts_alltime$Location == "Bel_2A"& dailywts_alltime$dailywt > 0)
dailywts_Bel_33 <- subset(dailywts_alltime, dailywts_alltime$Location == "Bel_33"& dailywts_alltime$dailywt > 0)
dailywts_Bel_34 <- subset(dailywts_alltime, dailywts_alltime$Location == "Bel_34"& dailywts_alltime$dailywt > 0)
dailywts_Bel_66 <- subset(dailywts_alltime, dailywts_alltime$Location == "Bel_66"& dailywts_alltime$dailywt > 0)
dailywts_Bel_TopCot <- subset(dailywts_alltime, dailywts_alltime$Location == "Bel_TopCot"& dailywts_alltime$dailywt > 0)
dailywts_DM12Final <- subset(dailywts_alltime, dailywts_alltime$Location == "DM12Final"& dailywts_alltime$dailywt > 0)
dailywts_DM13BelKraatz <- subset(dailywts_alltime, dailywts_alltime$Location == "DM13BelKraatz"& dailywts_alltime$dailywt > 0)
dailywts_Kraatz <- subset(dailywts_alltime, dailywts_alltime$Location == "Kraatz"& dailywts_alltime$dailywt > 0)

dailywts_herd1 <- get_dailywts(RFID = unique(dailywts_Bel_66$RFID))


ggplot(dailywts_alltime, aes(x = Date, y = dailywt, color = RFID)) +
  geom_line() +
  guides(color = "none")

ggplot(alltime_2016, aes(x = Date, y = dailywt, color = RFID)) +
  geom_point() +
  guides(color = "none")

ggplot(alltime_2017, aes(x = Date, y = dailywt, color = RFID)) +
  geom_point() +
  guides(color = "none")


dailywts_2015 <- dailywts_alltime[dailywts_alltime$Date <= "2015-12-31", ]
dailywts_2015_sep <- dailywts_2015[dailywts_2015$Date >= "2015-09-01", ]

RFID_03Bel <- unique(dailywts_alltime[dailywts_alltime$Location == "03.Bel", ]$RFID)
RFID_04Bel <- unique(dailywts_alltime[dailywts_alltime$Location == "04.Bel", ]$RFID)
RFID_06Bel <- unique(dailywts_alltime[dailywts_alltime$Location == "06.Bel", ]$RFID)
RFID_07Bel <- unique(dailywts_alltime[dailywts_alltime$Location == "07.Bel", ]$RFID)
RFID_09Bel <- unique(dailywts_alltime[dailywts_alltime$Location == "09.Bel", ]$RFID)
RFID_12Bel <- unique(dailywts_alltime[dailywts_alltime$Location == "12.Bel", ]$RFID)
RFID_13Bel <- unique(dailywts_alltime[dailywts_alltime$Location == "13.Bel", ]$RFID)
RFID_15Bel <- unique(dailywts_alltime[dailywts_alltime$Location == "15.Bel", ]$RFID)
RFID_Bel17 <- unique(dailywts_alltime[dailywts_alltime$Location == "Bel17", ]$RFID)
RFID_Bel24 <- unique(dailywts_alltime[dailywts_alltime$Location == "Bel24", ]$RFID)
RFID_Bel2ab <- unique(dailywts_alltime[dailywts_alltime$Location == "Bel2ab", ]$RFID)
RFID_Bel33 <- unique(dailywts_alltime[dailywts_alltime$Location == "Bel33", ]$RFID)
RFID_Bel34 <- unique(dailywts_alltime[dailywts_alltime$Location == "Bel34", ]$RFID)
RFID_Bel3B <- unique(dailywts_alltime[dailywts_alltime$Location == "Bel3B", ]$RFID)
RFID_Bel40 <- unique(dailywts_alltime[dailywts_alltime$Location == "Bel40", ]$RFID)
RFID_Bel65 <- unique(dailywts_alltime[dailywts_alltime$Location == "Bel65", ]$RFID)
RFID_Bel_17 <- unique(dailywts_alltime[dailywts_alltime$Location == "Bel_17", ]$RFID)
RFID_Bel_19 <- unique(dailywts_alltime[dailywts_alltime$Location == "Bel_19", ]$RFID)
RFID_Bel_2A <- unique(dailywts_alltime[dailywts_alltime$Location == "Bel_2A", ]$RFID)
RFID_Bel_33 <- unique(dailywts_alltime[dailywts_alltime$Location == "Bel_33", ]$RFID)
RFID_Bel_34 <- unique(dailywts_alltime[dailywts_alltime$Location == "Bel_34", ]$RFID)
RFID_Bel_66 <- unique(dailywts_alltime[dailywts_alltime$Location == "Bel_66", ]$RFID)
RFID_Bel_TopCot <- unique(dailywts_alltime[dailywts_alltime$Location == "Bel_TopCot", ]$RFID)
RFID_DM12Final <- unique(dailywts_alltime[dailywts_alltime$Location == "DM12Final", ]$RFID)
RFID_DM13BelKraatz <- unique(dailywts_alltime[dailywts_alltime$Location == "DM13BelKraatz", ]$RFID)
RFID_Kraatz <- unique(dailywts_alltime[dailywts_alltime$Location == "Kraatz", ]$RFID)


RFID_list <- list(c(RFID_03Bel, rep(NA, 100)), RFID_04Bel, RFID_06Bel, RFID_07Bel, RFID_09Bel, RFID_12Bel, RFID_13Bel, RFID_15Bel, RFID_Bel17, RFID_Bel24, RFID_Bel2ab)

RFID_df <- as.data.frame(RFID_list)



dialywts_03Bel <- dailywts_alltime[dailywts_alltime$Location == "03.Bel", ]


example_dailywts <- get_dailywts(RFID = "964 001010472687")
ggplot(example_dailywts, aes(x = Date, y = Weight, color = RFID)) +
  geom_point()



wow_herd1 <- dailywts_alltime[dailywts_alltime$Date >= "2015-03-20" & dailywts_alltime$Date <= "2020-01-30", ]
herd1_raw <- wow_herd1[wow_herd1$Weight > 0, ]

wow_herd2 <- dailywts_alltime[dailywts_alltime$Date >= "2020-09-08" & dailywts_alltime$Date <= "2022-04-17", ]
wow_herd3 <- dailywts_alltime[dailywts_alltime$Date >= "2022-09-02" & dailywts_alltime$Date <= "2023-04-29", ]

ggplot(dailywts_alltime, aes(x = Date, y = Weight, color = RFID)) +
  geom_point() +
  guides(color = "none")



calving_data <- get_calvingdata(property = "Belmont")
cattle_calf_hist <- get_cattlecalfhist(RFID = unique(dailywts_all$RFID))
cattle_preg_hist <- get_cattlepreghist(RFID = unique(dailywts_all$RFID))
dob_data <- get_DoBData(property = "Belmont")

DOB = c("2021-09-15", "2021-09-30", "2021-12-05", "2021-11-08", "2022-01-14", "2022-03-15", "2022-04-05", "2022-01-20", "2021-10-21", "2021-11-24")
example_data <- data.frame(ID = c(1:10), DOB = DOB)
example_data$day80 <- as.Date(DOB) + 80
example_data$day200 <- as.Date(DOB) + 200
example_data$day300 <- as.Date(DOB) + 300








ggplot(dailywts_raw, aes(x = Date, y = dailywt, color = RFID)) +
  geom_point() +
  guides(color = "none")

ggplot(staticwts_raw, aes(x = Date, y = staticwt, color = RFID)) +
  geom_point() +
  guides(color = "none")






