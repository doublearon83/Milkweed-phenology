#use data called HerbariumData_fl_use from herbarium_wrangling.Rmd using just the flowering data

library(flexsurv)
library(survival)
require(fitdistrplus)
library(dplyr)

############ finding estimate parameters ########################
surv_obj <- Surv(HerbariumData_fl_use$Day_of_Year, HerbariumData_fl_use$Phenophase_Status)

# Fit the Weibull regression model
# Covariates Latitude, Longitude, and Elevation_in_Meters 
fit <- flexsurvreg(surv_obj ~ Latitude + Longitude + Elevation_in_Meters, 
                   anc = list(shape = ~ Latitude + Longitude + Elevation_in_Meters), 
                   dist = "weibull", 
                   data = HerbariumData_fl_use)
# Display the coefficients of the model
coef(fit)

# Compute shape and scale parameters using coefficients
shape_param <- exp(coef(fit)["shape"] +
                     coef(fit)["shape(Latitude)"] * HerbariumData_fl_use$Latitude +
                     coef(fit)["shape(Longitude)"] * HerbariumData_fl_use$Longitude +
                     coef(fit)["shape(Elevation_in_Meters)"] * HerbariumData_fl_use$Elevation_in_Meters)

scale_param <- exp(coef(fit)["scale"] +
                     coef(fit)["Latitude"] * HerbariumData_fl_use$Latitude +
                     coef(fit)["Longitude"] * HerbariumData_fl_use$Longitude +
                     coef(fit)["Elevation_in_Meters"] * HerbariumData_fl_use$Elevation_in_Meters)


scale_shape <- data.frame(HerbariumData_fl_use$Observation_ID, shape_param, scale_param) %>% 
  rename(Observation_ID = HerbariumData_fl_use.Observation_ID )

##################################################################
############Run through for all percentiles (hard coded)##################

ni <- 100  # Set the number of iterations 
samp_size <- 1
percentile <- 0.01

new_data <- matrix(0, nrow = nrow(scale_shape), ncol = ni)

scale_shape$qweibull <- mapply(qweibull, percentile, scale_shape$shape_param, scale_shape$scale_param)

for (j in 1:ni) {
  random_samples_matrix <- mapply(function(shape, scale) qweibull(runif(samp_size), shape, scale),
                                  scale_shape$shape_param, scale_shape$scale_param, SIMPLIFY = FALSE)
  
  surv_obj_bias <- Surv(as.numeric(random_samples_matrix), HerbariumData_fl_use$Phenophase_Status)
  
  fit_bias <- flexsurvreg(surv_obj_bias ~ Latitude + Longitude + Elevation_in_Meters, 
                          anc = list(shape = ~ Latitude + Longitude + Elevation_in_Meters), 
                          dist = "weibull", 
                          data = HerbariumData_fl_use)
  
  shape_param_bias <- exp(coef(fit_bias)["shape"] +
                            coef(fit_bias)["shape(Latitude)"] * HerbariumData_fl_use$Latitude +
                            coef(fit_bias)["shape(Longitude)"] * HerbariumData_fl_use$Longitude +
                            coef(fit_bias)["shape(Elevation_in_Meters)"] * HerbariumData_fl_use$Elevation_in_Meters)
  
  scale_param_bias <- exp(coef(fit_bias)["scale"] +
                            coef(fit_bias)["Latitude"] * HerbariumData_fl_use$Latitude +
                            coef(fit_bias)["Longitude"] * HerbariumData_fl_use$Longitude +
                            coef(fit_bias)["Elevation_in_Meters"] * HerbariumData_fl_use$Elevation_in_Meters)
  
  qweibull_estimated <- mapply(qweibull, percentile, shape_param_bias, scale_param_bias)
  
  new_data[, j] <- qweibull_estimated
}

if (ni == 1) {
  column_means <- as.numeric(new_data)
} else {
  column_means <- rowMeans(new_data, na.rm = TRUE)
}

bias_results_herbarium_0.01 <- data.frame(
  Observation_ID = scale_shape$Observation_ID,
  Original = scale_shape$qweibull,
  Column_Means = column_means,
  Bias = column_means - scale_shape$qweibull,
  corrected_0.01 = 2*scale_shape$qweibull-column_means, #Belitz paper
  percentile = percentile
)



ni <- 100  # Set the number of iterations 
samp_size <- 1
percentile <- 0.5

new_data <- matrix(0, nrow = nrow(scale_shape), ncol = ni)

scale_shape$qweibull <- mapply(qweibull, percentile, scale_shape$shape_param, scale_shape$scale_param)

for (j in 1:ni) {
  random_samples_matrix <- mapply(function(shape, scale) qweibull(runif(samp_size), shape, scale),
                                  scale_shape$shape_param, scale_shape$scale_param, SIMPLIFY = FALSE)
  
  surv_obj_bias <- Surv(as.numeric(random_samples_matrix), HerbariumData_fl_use$Phenophase_Status)
  
  fit_bias <- flexsurvreg(surv_obj_bias ~ Latitude + Longitude + Elevation_in_Meters, 
                          anc = list(shape = ~ Latitude + Longitude + Elevation_in_Meters), 
                          dist = "weibull", 
                          data = HerbariumData_fl_use)
  
  shape_param_bias <- exp(coef(fit_bias)["shape"] +
                            coef(fit_bias)["shape(Latitude)"] * HerbariumData_fl_use$Latitude +
                            coef(fit_bias)["shape(Longitude)"] * HerbariumData_fl_use$Longitude +
                            coef(fit_bias)["shape(Elevation_in_Meters)"] * HerbariumData_fl_use$Elevation_in_Meters)
  
  scale_param_bias <- exp(coef(fit_bias)["scale"] +
                            coef(fit_bias)["Latitude"] * HerbariumData_fl_use$Latitude +
                            coef(fit_bias)["Longitude"] * HerbariumData_fl_use$Longitude +
                            coef(fit_bias)["Elevation_in_Meters"] * HerbariumData_fl_use$Elevation_in_Meters)
  
  qweibull_estimated <- mapply(qweibull, percentile, shape_param_bias, scale_param_bias)
  
  new_data[, j] <- qweibull_estimated
}

if (ni == 1) {
  column_means <- as.numeric(new_data)
} else {
  column_means <- rowMeans(new_data, na.rm = TRUE)
}

bias_results_herbarium_0.5 <- data.frame(
  Observation_ID = scale_shape$Observation_ID,
  Original = scale_shape$qweibull,
  Column_Means = column_means,
  Bias = column_means - scale_shape$qweibull,
  corrected_0.5 = 2*scale_shape$qweibull-column_means, #Belitz paper
  percentile = percentile
)

ni <- 100   
samp_size <- 1
percentile <- 0.99

new_data <- matrix(0, nrow = nrow(scale_shape), ncol = ni)

scale_shape$qweibull <- mapply(qweibull, percentile, scale_shape$shape_param, scale_shape$scale_param)

for (j in 1:ni) {
  random_samples_matrix <- mapply(function(shape, scale) qweibull(runif(samp_size), shape, scale),
                                  scale_shape$shape_param, scale_shape$scale_param, SIMPLIFY = FALSE)
  
  surv_obj_bias <- Surv(as.numeric(random_samples_matrix), HerbariumData_fl_use$Phenophase_Status)
  fit_bias <- flexsurvreg(surv_obj_bias ~ Latitude + Longitude + Elevation_in_Meters, 
                          anc = list(shape = ~ Latitude + Longitude + Elevation_in_Meters), 
                          dist = "weibull", 
                          data = HerbariumData_fl_use)
  
  shape_param_bias <- exp(coef(fit_bias)["shape"] +
                            coef(fit_bias)["shape(Latitude)"] * HerbariumData_fl_use$Latitude +
                            coef(fit_bias)["shape(Longitude)"] * HerbariumData_fl_use$Longitude +
                            coef(fit_bias)["shape(Elevation_in_Meters)"] * HerbariumData_fl_use$Elevation_in_Meters)
  
  scale_param_bias <- exp(coef(fit_bias)["scale"] +
                            coef(fit_bias)["Latitude"] * HerbariumData_fl_use$Latitude +
                            coef(fit_bias)["Longitude"] * HerbariumData_fl_use$Longitude +
                            coef(fit_bias)["Elevation_in_Meters"] * HerbariumData_fl_use$Elevation_in_Meters)
  
  qweibull_estimated <- mapply(qweibull, percentile, shape_param_bias, scale_param_bias)
  
  new_data[, j] <- qweibull_estimated
}

if (ni == 1) {
  column_means <- as.numeric(new_data)
} else {
  column_means <- rowMeans(new_data, na.rm = TRUE)
}

bias_results_herbarium_0.99 <- data.frame(
  Observation_ID = scale_shape$Observation_ID,
  Original = scale_shape$qweibull,
  Column_Means = column_means,
  Bias = column_means - scale_shape$qweibull,
  corrected_0.99 = 2*scale_shape$qweibull-column_means, #Belitz paper
  percentile = percentile
)


#Combine all data
bias_results_herbarium <- bind_rows(bias_results_herbarium_0.5, 
                              bias_results_herbarium_0.01, 
                              bias_results_herbarium_0.99)






















