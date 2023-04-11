##################### saving data for all propertie ####################################################

Killara_static_wts <- get_staticwts(property = "Killara")
Killara_staticwts <- Killara_static_wts[Killara_static_wts$Weight > 0, ]
write.csv(Killara_static_wts, file = "Data/All_properties/Killara_staticwts.csv")

Killara_daily_wts <- get_dailywts(property = "Killara")
Killara_dailywts <- Killara_daily_wts[Killara_daily_wts$Weight > 0, ]
Killara_dailywts$stationname <- "Killara"
write.csv(Killara_daily_wts, file = "Data/All_properties/Killara_dailywts.csv")


Belmont_static_wts <- get_staticwts(property = "Belmont")
Belmont_staticwts <- Belmont_static_wts[Belmont_static_wts$Weight > 0, ]
write.csv(Belmont_static_wts, file = "Data/All_properties/Belmont_staticwts.csv")

Belmont_daily_wts <- get_dailywts(property = "Belmont")
Belmont_dailywts <- Belmont_daily_wts[Belmont_daily_wts$Weight > 0, ]
Belmont_dailywts$stationname = "Belmont"
write.csv(Belmont_daily_wts, file = "Data/All_properties/Belmont_dailywts.csv")


Gyranda_static_wts <- get_staticwts(property = "Gyranda")
Gyranda_staticwts <- Gyranda_static_wts[Gyranda_static_wts$Weight > 0, ]
write.csv(Gyranda_static_wts, file = "Data/All_properties/Gyranda_staticwts.csv")

Gyranda_daily_wts <- get_dailywts(property = "Gyranda")
Gyranda_dailywts <- Gyranda_daily_wts[Gyranda_daily_wts$Weight > 0, ]
Gyranda_dailywts$stationname = "Gyranda"
write.csv(Gyranda_daily_wts, file = "Data/All_properties/Gyranda_dailywts.csv")


CQIRP_static_wts <- get_staticwts(property = "CQIRP")
CQIRP_staticwts <- CQIRP_static_wts[CQIRP_static_wts$Weight > 0, ]
write.csv(CQIRP_static_wts, file = "Data/All_properties/CQIRP_staticwts.csv")

CQIRP_daily_wts <- get_dailywts(property = "CQIRP")
CQIRP_dailywts <- CQIRP_daily_wts[CQIRP_daily_wts$Weight > 0, ]
CQIRP_dailywts$stationname = "CQIRP"
write.csv(CQIRP_daily_wts, file = "Data/All_properties/CQIRP_dailywts.csv")


Berrigurra_static_wts <- get_staticwts(property = "Berrigurra")
Berrigurra_staticwts <- Berrigurra_static_wts[Berrigurra_static_wts$Weight > 0, ]
write.csv(Berrigurra_static_wts, file = "Data/All_properties/Berrigurra_staticwts.csv")

Berrigurra_daily_wts <- get_dailywts(property = "Berrigurra")
Berrigurra_dailywts <- Berrigurra_daily_wts[Berrigurra_daily_wts$Weight > 0, ]
Berrigurra_dailywts$stationname <- "Berrigurra"
write.csv(Berrigurra_daily_wts, file = "Data/All_properties/Berrigurra_dailywts.csv")


Anguil_static_wts <- get_staticwts(property = "Anguil")
Anguil_staticwts <- Anguil_static_wts[Anguil_static_wts$Weight > 0, ]
write.csv(Anguil_static_wts, file = "Data/All_properties/Anguil_staticwts.csv")

Anguil_daily_wts <- get_dailywts(property = "Anguil")
Anguil_dailywts <- Anguil_daily_wts[Anguil_daily_wts$Weight > 0, ]
Anguil_dailywts$stationname <- "Anguil"
write.csv(Anguil_daily_wts, file = "Data/All_properties/Anguil_dailywts.csv")

Belcarce_static_wts <- get_staticwts(property = "Belcarce")
Belcarce_staticwts <- Belcarce_static_wts[Belcarce_static_wts$Weight > 0, ]
write.csv(Belcarce_static_wts, file = "Data/All_properties/Belcarce_staticwts.csv")

Belcarce_daily_wts <- get_dailywts(property = "Belcarce")
Belcarce_dailywts <- Belcarce_daily_wts[Belcarce_daily_wts$Weight > 0, ]
Belcarce_dailywts$stationname <- "Belcarce"
write.csv(Belcarce_daily_wts, file = "Data/All_properties/Belcarce_dailywts.csv")


ElJaguel_static_wts <- get_staticwts(property = "El Jaguel")
ElJaguel_staticwts <- ElJaguel_static_wts[ElJaguel_static_wts$Weight > 0, ]
write.csv(ElJaguel_static_wts, file = "Data/All_properties/ElJaguel_staticwts.csv")

ElJaguel_daily_wts <- get_dailywts(property = "El Jaguel")
ElJaguel_dailywts <- ElJaguel_daily_wts[ElJaguel_daily_wts$Weight > 0, ]
ElJaguel_dailywts$stationname <- "El Jaguel"
write.csv(ElJaguel_daily_wts, file = "Data/All_properties/ElJaguel_dailywts.csv")


LaEsmeralda_static_wts <- get_staticwts(property = "La Esmeralda")
LaEsmeralda_staticwts <- LaEsmeralda_static_wts[LaEsmeralda_static_wts$Weight > 0, ]
write.csv(LaEsmeralda_static_wts, file = "Data/All_properties/LaEsmeralda_staticwts.csv")

LaEsmeralda_daily_wts <- get_dailywts(property = "La Esmeralda")
LaEsmeralda_dailywts <- LaEsmeralda_daily_wts[LaEsmeralda_daily_wts$Weight > 0, ]
LaEsmeralda_dailywts$stationname <- "La Esmeralda"
write.csv(LaEsmeralda_daily_wts, file = "Data/All_properties/LaEsmeralda_dailywts.csv")


Carisma_static_wts <- get_staticwts(property = "Carisma")
Carisma_staticwts <- Carisma_static_wts[Carisma_static_wts$Weight > 0, ]
write.csv(Carisma_static_wts, file = "Data/All_properties/Carisma_staticwts.csv")

Carisma_daily_wts <- get_dailywts(property = "Carisma")
Carisma_dailywts <- Carisma_daily_wts[Carisma_daily_wts$Weight > 0, ]
Carisma_dailywts$stationname <- "Carisma"
write.csv(Carisma_daily_wts, file = "Data/All_properties/Carisma_dailywts.csv")


LaEmilia_static_wts <- get_staticwts(property = "La Emilia")
LaEmilia_staticwts <- LaEmilia_static_wts[LaEmilia_static_wts$Weight > 0, ]
write.csv(LaEmilia_static_wts, file = "Data/All_properties/LaEmilia_staticwts.csv")

LaEmilia_daily_wts <- get_dailywts(property = "La Emilia")
LaEmilia_dailywts <- LaEmilia_daily_wts[LaEmilia_daily_wts$Weight > 0, ]
LaEmilia_dailywts$stationname <- "La Emilia"
write.csv(LaEmilia_daily_wts, file = "Data/All_properties/LaEmilia_dailywts.csv")

Mathison_static_wts <- get_staticwts(property = "Mathison")
Mathison_staticwts <- Mathison_static_wts[Mathison_static_wts$Weight > 0, ]
write.csv(Mathison_static_wts, file = "Data/All_properties/Mathison_staticwts.csv")

Mathison_daily_wts <- get_dailywts(property = "Mathison")
Mathison_dailywts <- Mathison_daily_wts[Mathison_daily_wts$Weight > 0, ]
Mathison_dailywts$stationname <- "Mathison"
write.csv(Mathison_daily_wts, file = "Data/All_properties/Mathison_dailywts.csv")


LaFlorida_static_wts <- get_staticwts(property = "La Florida")
LaFlorida_staticwts <- LaFlorida_static_wts[LaFlorida_static_wts$Weight > 0, ]
write.csv(LaFlorida_static_wts, file = "Data/All_properties/LaFlorida_staticwts.csv")

LaFlorida_daily_wts <- get_dailywts(property = "La Florida")
LaFlorida_dailywts <- LaFlorida_daily_wts[LaFlorida_daily_wts$Weight > 0, ]
LaFlorida_dailywts$stationname <- "La Florida"
write.csv(LaFlorida_daily_wts, file = "Data/All_properties/LaFlorida_dailywts.csv")


Glenmoan_static_wts <- get_staticwts(property = "Glenmoan")
Glenmoan_staticwts <- Glenmoan_static_wts[Glenmoan_static_wts$Weight > 0, ]
write.csv(Glenmoan_static_wts, file = "Data/All_properties/Glenmoan_staticwts.csv")

Glenmoan_daily_wts <- get_dailywts(property = "Glenmoan")
Glenmoan_dailywts <- Glenmoan_daily_wts[Glenmoan_daily_wts$Weight > 0, ]
Glenmoan_dailywts$stationname <- "Glenmoan"
write.csv(Glenmoan_daily_wts, file = "Data/All_properties/Glenmoan_dailywts.csv")


LaFermina_static_wts <- get_staticwts(property = "La Fermina")
LaFermina_staticwts <- LaFermina_static_wts[LaFermina_static_wts$Weight > 0, ]
write.csv(LaFermina_static_wts, file = "Data/All_properties/LaFermina_staticwts.csv")

LaFermina_daily_wts <- get_dailywts(property = "La Fermina")
LaFermina_dailywts <- LaFermina_daily_wts[LaFermina_daily_wts$Weight > 0, ]
LaFermina_dailywts$stationname <- "La Fermina"
write.csv(LaFermina_daily_wts, file = "Data/All_properties/LaFermina_dailywts.csv")


LaTijereta_static_wts <- get_staticwts(property = "La Tijereta")
LaTijereta_staticwts <- LaTijereta_static_wts[LaTijereta_static_wts$Weight > 0, ]
write.csv(LaTijereta_static_wts, file = "Data/All_properties/LaTijereta_staticwts.csv")

LaTijereta_daily_wts <- get_dailywts(property = "La Tijereta")
LaTijereta_dailywts <- LaTijereta_daily_wts[LaTijereta_daily_wts$Weight > 0, ]
LaTijereta_dailywts$stationname <- "La Tijereta"
write.csv(LaTijereta_daily_wts, file = "Data/All_properties/LaTijereta_dailywts.csv")


CorfieldDowns_static_wts <- get_staticwts(property = "Corfield Downs")
CorfieldDowns_staticwts <- CorfieldDowns_static_wts[CorfieldDowns_static_wts$Weight > 0, ]
write.csv(CorfieldDowns_static_wts, file = "Data/All_properties/CorfieldDowns_staticwts.csv")

CorfieldDowns_daily_wts <- get_dailywts(property = "Corfield Downs")
CorfieldDowns_dailywts <- CorfieldDowns_daily_wts[CorfieldDowns_daily_wts$Weight > 0, ]
CorfieldDowns_dailywts$stationname <- "Corfield Downs"
write.csv(CorfieldDowns_daily_wts, file = "Data/All_properties/CorfieldDowns_dailywts.csv")


Tremere_static_wts <- get_staticwts(property = "Tremere")
Tremere_staticwts <- Tremere_static_wts[Tremere_static_wts$Weight > 0, ]
write.csv(Tremere_static_wts, file = "Data/All_properties/Tremere_staticwts.csv")

Tremere_daily_wts <- get_dailywts(property = "Tremere")
Tremere_dailywts <- Tremere_daily_wts[Tremere_daily_wts$Weight > 0, ]
Tremere_dailywts$stationname <- "Tremere"
write.csv(Tremere_daily_wts, file = "Data/All_properties/Tremere_dailywts.csv")


ValleInferior_static_wts <- get_staticwts(property = "Valle Inferior")
ValleInferior_staticwts <- ValleInferior_static_wts[ValleInferior_static_wts$Weight > 0, ]
write.csv(ValleInferior_static_wts, file = "Data/All_properties/ValleInferior_staticwts.csv")

ValleInferior_daily_wts <- get_dailywts(property = "Valle Inferior")
ValleInferior_dailywts <- ValleInferior_daily_wts[ValleInferior_daily_wts$Weight > 0, ]
ValleInferior_dailywts$stationname <- "Valle Inferior"
write.csv(ValleInferior_daily_wts, file = "Data/All_properties/ValleInferior_dailywts.csv")