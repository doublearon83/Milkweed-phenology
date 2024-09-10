# Runs a generalized weibull regression that estimates how 
# weibull shape and scale parameters change with covariates

install.packages("flexsurv")
library(flexsurv)

set.seed(123)

# Number of observations
n <- 100

# Covariates
X1 <- rnorm(n, mean = 50, sd = 10)  # Covariate for scale parameter
X2 <- rnorm(n, mean = 0, sd = 1)    # Covariate for shape parameter

# True Weibull parameters
true_shape <- 2
true_scale <- 50

# Simulate Weibull-distributed survival times
lambda <- exp(0.1 * X1 + 0.05 * X2)                     # Scale parameter varies with X1 and X2
shape <- exp(0.05 * X1 + 0.02 * X2 + log(true_shape))   # Shape parameter varies with X3 and X4
T <- lambda * (-log(runif(n)))^(1 / shape)              # Simulated survival times

# Create a data frame
data <- data.frame(T, X1, X2)


# Fit the Weibull regression model with covariates affecting both scale and shape
fit <- flexsurvreg(Surv(T) ~ X1 + X2, anc = list(shape = ~ X1 + X2), dist = "weibull", data = data)

# coefs of the model fit
coef(fit)

#How shape and scale change with covariates
exp(coef(fit)[1]+coef(fit)[5]*data$X1[1]+coef(fit)[6]*data$X2[1])
exp(coef(fit)[2]+coef(fit)[3]*data$X1[1]+coef(fit)[4]*data$X2[1])
    