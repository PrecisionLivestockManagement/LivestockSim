library(lme4)

# create a data frame with columns for animal ID, weight, and measurement occasion
data <- data.frame(
  animal = rep(1:10, each = 3),
  weight = c(56, 58, 57, 45, 47, 46, 68, 70, 69, 82, 84, 83, 62, 64, 63, 73, 75, 74, 53, 55, 54,
             61, 63, 62, 48, 50, 49, 79, 81, 80),
  occasion = rep(1:3, times = 10)
)

# fit the two-way random effects model using lmer
model <- lmer(weight ~ 1 + (1 | animal), data = data, REML = TRUE)

# print the summary of the model to see the estimated variance components
summary(model)

# calculate the repeatability as R = Va / (Va + Ve)
Vi <- as.numeric(VarCorr(model)$animal[1])
Vr <- as.numeric(attr(VarCorr(model), "sc")^2)
R <- Vi / (Vi + Vr)


# Load the cccrm package
library(cccrm)

# Create example data
method1 <- c(10, 11, 12, 13, 14, 15)
method2 <- c(9, 11, 12, 13, 15, 16)

# Calculate the concordance correlation coefficient
ccclonw(method1, method2)

