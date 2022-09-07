#calculation of liveweight from birth (Jan 1) to year 1 (Dec 30)

n <- 100
ID <- c(01001:01100)
BW <- rnorm(n, 50, 10) #birth weight
DWG <- rnorm(n, 0.8, 0.4) #daily weight gain
GP <- runif(n, min = 0.1, max = 0.9) #genetic potential
d <- c(1:365)

#calculation of weights at day d

LWT <- function(d) {
  BW + DWG *d
}


for (i in ID) {
  WT<- BW + DWG *d
  Print(WT[i])
}

result <- cbind(ID, GP, BW, DWG)
