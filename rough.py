
# BW = 50
# GP = 0.5
# FA = int
# month = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

# # DWG = BW/50 + GP + FA/6

# for i in month:
#     if i == month[0]:
#         FA = 2
#     elif i == month[1]:
#         FA = 3
#     else:
#         FA = 4
        
#     DWG = BW/50 + GP + FA/6
#     print(DWG)   
        
        
# if month[0]: 
#     FA =2
# if month[1]: 
#     FA =3
# if month[2]: 
#     FA =4
# if month[3]: 
#     FA =7
# if month[4]: 
#     FA =8
# if month[5]: 
#     FA =5
# if month[6]: 
#     FA =6
# if month[7]: 
#     FA =9
# if month[8]: 
#     FA =10
# if month[9]: 
#     FA =12
# if month[10]: 
#     FA =11
# if month[11]: 
#     FA =1

# import numpy as np

# BW = np.random.normal(50, 10)
# GP = np.random.uniform(0.1, 0.9, 2)

# print(GP)
# print(BW)

# # this simulates the genetic merit of individual animal
# set.seed(1)
# n <- 100 # initial herd size

# id <- c(01001:01100)
# age <- runif(n, min = 12, max = 120) # in months
# daily_wt <- rnorm(n, 0.8, 0.4) # in kgs
# fertility <- rnorm(n, 0.9, 0.2) 

# cow_attributes <- cbind(id, age, daily_wt, fertility)


# # linear model to calculate genetic merit
# genetic_merit <- age * 0.08 + daily_wt + fertility

# data_table <- cbind(cow_attributes, genetic_merit)


# # simulation runs
# reps <- 1000
# sim_result <- rep(NA, reps)

# for(i in 1:reps){
#   sample <- rnorm(n, mean = mean(genetic_merit), sd = sd(genetic_merit))
#   sim_result[i] <- mean(sample)
# }
# sim_result

# # MOnte carlo
# n = 100
# # define function
# final_wt <- function(n, mean, sd){
#   birth_wt <- rnorm (n, 40, 7.5)
#   daily_wt <- rnorm(n, 0.8, 0.4)
#   final_wt <- mean(birth_wt) + mean(daily_wt)*365
#   return(list("final_wt" = final_wt))
# }

# n_grid <- 100
# mean_grid <- 40
# sd_grid <- 7.5

# param_list = list("n"=n_grid, "mean"=mean_grid, "sd"= sd_grid)

# result <- MonteCarlo(func = final_wt, nrep = 1000, param_list = param_list)
import math

SRW = 500
CN1 = 0.0157
CN2 = 0.27
BW = 50

def calc(A):
    N = SRW -((SRW-BW) * math.exp((-CN1 * A)/(pow(SRW, CN2))))
    print (N)
 
i = input("Enter day:")
i = int(i) 


calc(i)
