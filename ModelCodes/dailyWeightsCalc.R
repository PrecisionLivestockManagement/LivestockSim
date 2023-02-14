# Importing Packages
library(tidyverse)

ID = 1001

# BW = readline("Birth weight:")
# BW = as.numeric(BW)
BW = 50

# GP = readline("Genetic potential:")
# GP = as.numeric(GP)
GP = 1

Month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

FA = c(Jan = 2, Feb = 3, Mar = 4, Apr = 7, May = 8, Jun = 5, Jul = 6, Aug = 9, Sep = 10, Oct = 12, Nov = 11, Dec = 1)

# Assuming an animal is born in 1 Jan 
# Daily wt gain for each month
DWG_Jan = BW/50 + GP + FA["Jan"]/6
DWG_Feb = BW/50 + GP + FA["Feb"]/6
DWG_Mar = BW/50 + GP + FA["Mar"]/6
DWG_Apr = BW/50 + GP + FA["Apr"]/6
DWG_May = BW/50 + GP + FA["May"]/6
DWG_Jun = BW/50 + GP + FA["Jun"]/6
DWG_Jul = BW/50 + GP + FA["Jul"]/6
DWG_Aug = BW/50 + GP + FA["Aug"]/6
DWG_Sep = BW/50 + GP + FA["Sep"]/6
DWG_Oct = BW/50 + GP + FA["Oct"]/6
DWG_Nov = BW/50 + GP + FA["Nov"]/6
DWG_Dec = BW/50 + GP + FA["Dec"]/6

######### animal weights calculation for the month of January ###########
WT_Jan = c()
for (i in 1:31){ 
  CALC_Jan = BW + DWG_Jan * (i - 1)
  WT_Jan = c(WT_Jan, round(CALC_Jan, 1))
}
FWT_Jan = WT_Jan[31]

######### animal weights calculation for the month of February ##########
WT_Feb = c()
for (i in 1:28){ 
  CALC_Feb = FWT_Jan + DWG_Feb * i
  WT_Feb = c(WT_Feb, round(CALC_Feb, 1))
}
FWT_Feb = WT_Feb[28]

######### animal weights calculation for the month of March ###########
WT_Mar = c()
for (i in 1:31){ 
  CALC_Mar = FWT_Feb + DWG_Mar * i
  WT_Mar = c(WT_Mar, round(CALC_Mar, 1))
}
FWT_Mar = WT_Mar[31]

######### animal weights calculation for the month of April ###########
WT_Apr = c()
for (i in 1:30){ 
  CALC_Apr = FWT_Mar + DWG_Apr * i
  WT_Apr = c(WT_Apr, round(CALC_Apr, 1))
}
FWT_Apr = WT_Apr[30]

######### animal weights calculation for the month of May ###########
WT_May = c()
for (i in 1:31){ 
  CALC_May = FWT_Apr + DWG_May * i
  WT_May = c(WT_May, round(CALC_May, 1))
}
FWT_May = WT_May [31]

######### animal weights calculation for the month of June ###########
WT_Jun = c()
for (i in 1:30){ 
  CALC_Jun = FWT_May + DWG_Jun * i
  WT_Jun = c(WT_Jun, round(CALC_Jun, 1))
}
FWT_Jun = WT_Jun [30]

######### animal weights calculation for the month of July ###########
WT_Jul = c()
for (i in 1:31){ 
  CALC_Jul = FWT_Jun + DWG_Jul * i
  WT_Jul = c(WT_Jul, round(CALC_Jul, 1))
}
FWT_Jul = WT_Jul [31]

######### animal weights calculation for the month of August ###########
WT_Aug = c()
for (i in 1:31){ 
  CALC_Aug = FWT_Jul + DWG_Aug * i
  WT_Aug = c(WT_Aug, round(CALC_Aug, 1))
}
FWT_Aug = WT_Aug [31]

######### animal weights calculation for the month of September ###########
WT_Sep = c()
for (i in 1:30){ 
  CALC_Sep = FWT_Aug + DWG_Sep * i
  WT_Sep = c(WT_Sep, round(CALC_Sep, 1))
}
FWT_Sep = WT_Sep [30]

######### animal weights calculation for the month of October ###########
WT_Oct = c()
for (i in 1:31){ 
  CALC_Oct = FWT_Sep + DWG_Oct * i
  WT_Oct = c(WT_Oct, round(CALC_Oct, 1))
}
FWT_Oct = WT_Oct [31]

######### animal weights calculation for the month of November ###########
WT_Nov = c()
for (i in 1:30){ 
  CALC_Nov = FWT_Oct + DWG_Nov * i
  WT_Nov = c(WT_Nov, round(CALC_Nov, 1))
}
FWT_Nov = WT_Nov [30]

######### animal weights calculation for the month of December ###########
WT_Dec = c()
for (i in 1:31){ 
  CALC_Dec = FWT_Nov + DWG_Dec * i
  WT_Dec = c(WT_Dec, round(CALC_Dec, 1))
}
FWT_Dec = WT_Dec [31]


data <- data.frame(Jan = WT_Jan, Feb = c(WT_Feb, NA, NA, NA), Mar = WT_Mar, Apr = c(WT_Apr, NA), May = WT_May, Jun = c(WT_Jun, NA), Jul = WT_Jul, Aug = WT_Aug, Sep = c(WT_Sep, NA), Oct = WT_Oct, Nov = c(WT_Nov, NA), Dec = WT_Dec)


########## Stochastic prediction #############

install.packages("ggplot")
library(ggplot2)

library(ggplot2)

set.seed(123) # set seed for reproducibility
n_sim <- 100 # number of simulations
days <- 100 # number of days
birth_weight <- 50 # birth weight
genetic_potential <- 0.5 # genetic potential

sim_weights <- data.frame(sim = numeric(n_sim), day = numeric(n_sim*days), weight = numeric(n_sim*days)) # data frame to store the results

for (i in 1:n_sim) {
  weight <- numeric(days)
  weight[1] <- birth_weight
  for (d in 2:days) {
    weight[d] <- weight[d-1] + genetic_potential
  }
  sim_weights[(i-1)*days + 1:days, ] <- data.frame(sim = rep(i, days), day = 1:days, weight = weight)
}

ggplot(sim_weights, aes(x=day, y=weight, color=factor(sim))) + 
  geom_line() +
  xlab("Day") +
  ylab("Weight") +
  ggtitle("100 simulations of animal's weight growth")



#######################################

set.seed(123)

birth_weight <- 50
genetic_potential <- 0.5
days <- 100
n_simulations <- 10

weights <- data.frame(simulation = numeric(0),
                      day = numeric(0),
                      weight = numeric(0))

for (i in 1:n_simulations) {
  env_factor <- rnorm(days, mean = 0, sd = 10)
  daily_weights <- birth_weight + genetic_potential + env_factor
  cumulative_weights <- cumsum(daily_weights)
  sim_weights <- data.frame(simulation = rep(i, days),
                            day = 1:days,
                            weight = cumulative_weights + birth_weight)
  weights <- rbind(weights, sim_weights)
}

library(ggplot2)
ggplot(weights, aes(x = day, y = weight, color = as.factor(simulation))) +
  geom_line() +
  xlab("Day") +
  ylab("Weight (kg)") +
  ggtitle("Simulated Animal Weight Trajectories")




