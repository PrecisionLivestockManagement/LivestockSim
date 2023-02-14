
######### this simulates the genetic merit of individual animal ###########
set.seed(1)
n <- 100 # initial herd size
id <- c(01001:01100)
age <- runif(n, min = 12, max = 120) # in months
daily_wt <- rnorm(n, 0.8, 0.4) # in kgs
fertility <- rnorm(n, 0.9, 0.2)
cow_attributes <- cbind(id, age, daily_wt, fertility)

# linear model to calculate genetic merit
genetic_merit <- age * 0.08 + daily_wt + fertility
data_table <- cbind(cow_attributes, genetic_merit)

# simulation runs
reps <- 1000
sim_result <- rep(NA, reps)
for(i in 1:reps){
  sample <- rnorm(n, mean = mean(genetic_merit), sd = sd(genetic_merit))
  sim_result[i] <- mean(sample)
}
sim_result


########### final weight simulation #############
n = 100
# define function
final_wt <- function(n, mean, sd){
  birth_wt <- rnorm (n, 40, 7.5)
  daily_wt <- rnorm(n, 0.8, 0.4)
  final_wt <- mean(birth_wt) + mean(daily_wt)*365
  return(list("final_wt" = final_wt))
}
n_grid <- 100
mean_grid <- 40
sd_grid <- 7.5
param_list = list("n"=n_grid, "mean"=mean_grid, "sd"= sd_grid)
result <- MonteCarlo(func = final_wt, nrep = 1000, param_list = param_list)
result

