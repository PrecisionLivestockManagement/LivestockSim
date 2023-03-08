# plotting the growth curve using Bordy equation
library(ggplot2)

age <- c(0, 100, 200, 300, 400, 500, 600) # in days
weight <- c(40, 150, 250, 350, 400, 450, 500 ) # in kg

# defining Brody growth curve model
model <- function(A, B, k, t) {
  A * (1 - B * exp(-k * t))
}

#where, A = asymptotic weight of an animal (mature weight can be used)
      #B = integration constant (growth acceleration coefficient)
      # K = maturing rate or growth rate coefficient
      # t = age in days

# estimating growth parameters using nonlinear regression (non-linear least square function)
model_fit <- nls(weight ~ model(A, B, k, age),
                 start = list(A = 500, B = 0.5, k = 0.01))

summary(model_fit)

# A, B and k values obtained from model fit
A <- 692.7 # in kg
B <- 0.95
k <- 0.002

data <- data.frame(age, weight)
ggplot(data, aes(x = age, y = weight)) +
  geom_line()


#simulation
# age <- c(1:600)
# for (t in age) {
#   wts <- model(A, B, k, t)
#   
#   data <- data.frame(age, wts)
# }
# 
# ggplot(data, aes(x = age, y = wts)) +
#   geom_line()
# 
# # plotting the growth curve
# data <- data.frame(age, weight, A, B, k)
# ggplot(data, aes(x = age, y = weight)) +
#   geom_line()


#### simulating daily weight based on above model #####
# n <- 100 # no of simulations
# 
# for (i in age) {
#   all_age <- c(rep(age, each = 100))
# }
# 
# # let weights at different days
# wt_0 <- rnorm(n, 50, 10)
# wt_100 <- rnorm(n, 150, 50)
# wt_200 <- rnorm(n, 250, 50)
# wt_300 <- rnorm(n, 350, 50)
# wt_400 <- rnorm(n, 400, 50)
# wt_500 <- rnorm(n, 450, 50)
# wt_600 <- rnorm(n, 500, 50)
# 
# weights <- c(wt_0, wt_100, wt_200, wt_300, wt_400, wt_500, wt_600)
# 
# all_data <- data.frame(all_age, weights)
# 
# ggplot(all_data, aes(x = all_age, y = weights)) +
#   geom_point()
# 
# 
