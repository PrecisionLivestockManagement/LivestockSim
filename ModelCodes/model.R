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

# plotting the growth curve
data <- data.frame(age, weight, A, B, k)
ggplot(data, aes(x = age, y = weight)) +
  geom_line()



