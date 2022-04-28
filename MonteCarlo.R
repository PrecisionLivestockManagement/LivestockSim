
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
