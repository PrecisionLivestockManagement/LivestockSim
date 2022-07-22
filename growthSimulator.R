# Birth weigth in Kg
BW <- 40

# daily weight gain in kg
WG <- 0.8

#weight of an animal on day d
WT <- function(d){
  LWT <- BW + WG * d
  return(LWT)
}

#calculatio of liveweight from birth (day 0) to death (day 3650)
