library(flexsurv)
library(survival)
require(fitdistrplus)
library(dplyr)

############ finding estimate parameters ########################
 NPN_surv_obj <- Surv(phen_data_of$Day_of_Year, phen_data_of$Phenophase_Status)

#  NPN_fit the Weibull regression model
# Covariates Latitude, Longitude, and Elevation_in_Meters 
   NPN_fit <- flexsurvreg( NPN_surv_obj ~ Latitude + Longitude + Elevation_in_Meters + Year + gs_temp + gs_precip , 
                   anc = list(shape = ~ Latitude + Longitude + Elevation_in_Meters + Year + gs_temp + gs_precip), 
                   dist = "weibull", 
                   data = phen_data_of)
# Display the coefficients of the model
coef( NPN_fit)

# Compute shape and scale parameters using coefficients
NPN_shape_param <- exp(coef( NPN_fit)["shape"] +
                     coef( NPN_fit)["shape(Latitude)"] * phen_data_of$Latitude +
                     coef( NPN_fit)["shape(Longitude)"] * phen_data_of$Longitude +
                     coef( NPN_fit)["shape(Elevation_in_Meters)"] * phen_data_of$Elevation_in_Meters +
                     coef( NPN_fit)["shape(Year)"] * phen_data_of$Year +
                     coef( NPN_fit)["shape(gs_temp)"] * phen_data_of$gs_temp + 
                     coef( NPN_fit)["shape(gs_precip)"] * phen_data_of$gs_precip)
  

NPN_scale_param <- exp(coef( NPN_fit)["scale"] +
                     coef( NPN_fit)["Latitude"] * phen_data_of$Latitude +
                     coef( NPN_fit)["Longitude"] * phen_data_of$Longitude +
                     coef( NPN_fit)["Elevation_in_Meters"] * phen_data_of$Elevation_in_Meters +
                     coef( NPN_fit)["Year"] * phen_data_of$Year +
                     coef( NPN_fit)["gs_temp"] * phen_data_of$gs_temp + 
                     coef( NPN_fit)["gs_precip"] * phen_data_of$gs_precip)


NPN_scale_shape <- data.frame(phen_data_of$Observation_ID, NPN_shape_param, NPN_scale_param) %>% 
  rename(Observation_ID = phen_data_of.Observation_ID )

###########################################################################
############Run through for all percentiles (hard coded)##################

ni <- 100  # Set the number of iterations 
samp_size <- 1
percentile <- 0.01

new_data <- matrix(0, nrow = nrow(NPN_scale_shape), ncol = ni)

NPN_scale_shape$qweibull <- mapply(qweibull, percentile, NPN_scale_shape$NPN_shape_param, NPN_scale_shape$NPN_scale_param)

for (j in 1:ni) {
  NPN_random_samples_matrix <- mapply(function(shape, scale) qweibull(runif(samp_size), shape, scale),
                                  NPN_scale_shape$NPN_shape_param, NPN_scale_shape$NPN_scale_param, SIMPLIFY = FALSE)
  
   NPN_surv_obj_bias <- Surv(as.numeric(NPN_random_samples_matrix), phen_data_of$Phenophase_Status)
  
   NPN_fit_bias <- flexsurvreg( NPN_surv_obj_bias ~ Latitude + Longitude + Elevation_in_Meters  + Year + gs_temp + gs_precip, 
                          anc = list(shape = ~ Latitude + Longitude + Elevation_in_Meters + Year + gs_temp + gs_precip), 
                          dist = "weibull", 
                          data = phen_data_of)
  
  NPN_shape_param_bias <- exp(coef( NPN_fit_bias)["shape"] +
                            coef( NPN_fit_bias)["shape(Latitude)"] * phen_data_of$Latitude +
                            coef( NPN_fit_bias)["shape(Longitude)"] * phen_data_of$Longitude +
                            coef( NPN_fit_bias)["shape(Elevation_in_Meters)"] * phen_data_of$Elevation_in_Meters  +
                            coef( NPN_fit)["shape(Year)"] * phen_data_of$Year +
                            coef( NPN_fit)["shape(gs_temp)"] * phen_data_of$gs_temp + 
                            coef( NPN_fit)["shape(gs_precip)"] * phen_data_of$gs_precip)
  
  NPN_scale_param_bias <- exp(coef( NPN_fit_bias)["scale"] +
                            coef( NPN_fit_bias)["Latitude"] * phen_data_of$Latitude +
                            coef( NPN_fit_bias)["Longitude"] * phen_data_of$Longitude +
                            coef( NPN_fit_bias)["Elevation_in_Meters"] * phen_data_of$Elevation_in_Meters  +
                            coef( NPN_fit)["Year"] * phen_data_of$Year +
                            coef( NPN_fit)["gs_temp"] * phen_data_of$gs_temp + 
                            coef( NPN_fit)["gs_precip"] * phen_data_of$gs_precip)
  
  NPN_qweibull_estimated <- mapply(qweibull, percentile, NPN_shape_param_bias, NPN_scale_param_bias)
  
  new_data[, j] <- NPN_qweibull_estimated
}

if (ni == 1) {
  column_means <- new_data
} else {
  column_means <- colMeans(new_data, na.rm = TRUE)
}

bias_results_NPN_0.01 <- data.frame(
  Observation_ID = NPN_scale_shape$Observation_ID,
  Original = NPN_scale_shape$qweibull,
  Column_Means = column_means,
  Bias = column_means - NPN_scale_shape$qweibull,
  corrected_0.01 = 2*NPN_scale_shape$qweibull-column_means, #Belitz paper
  percentile = percentile
)


ni <- 100  # Set the number of iterations 
samp_size <- 1
percentile <- 0.5

new_data <- matrix(0, nrow = nrow(NPN_scale_shape), ncol = ni)

NPN_scale_shape$qweibull <- mapply(qweibull, percentile, NPN_scale_shape$NPN_shape_param, NPN_scale_shape$NPN_scale_param)

for (j in 1:ni) {
  NPN_random_samples_matrix <- mapply(function(shape, scale) qweibull(runif(samp_size), shape, scale),
                                  NPN_scale_shape$NPN_shape_param, NPN_scale_shape$NPN_scale_param, SIMPLIFY = FALSE)
  
   NPN_surv_obj_bias <- Surv(as.numeric(NPN_random_samples_matrix), phen_data_of$Phenophase_Status)
  
   NPN_fit_bias <- flexsurvreg( NPN_surv_obj_bias ~ Latitude + Longitude + Elevation_in_Meters + Year + gs_temp + gs_precip, 
                          anc = list(shape = ~ Latitude + Longitude + Elevation_in_Meters + Year + gs_temp + gs_precip), 
                          dist = "weibull", 
                          data = phen_data_of)
  
  NPN_shape_param_bias <- exp(coef( NPN_fit_bias)["shape"] +
                            coef( NPN_fit_bias)["shape(Latitude)"] * phen_data_of$Latitude +
                            coef( NPN_fit_bias)["shape(Longitude)"] * phen_data_of$Longitude +
                            coef( NPN_fit_bias)["shape(Elevation_in_Meters)"] * phen_data_of$Elevation_in_Meters  +
                            coef( NPN_fit)["shape(Year)"] * phen_data_of$Year +
                            coef( NPN_fit)["shape(gs_temp)"] * phen_data_of$gs_temp + 
                            coef( NPN_fit)["shape(gs_precip)"] * phen_data_of$gs_precip)
  
  NPN_scale_param_bias <- exp(coef( NPN_fit_bias)["scale"] +
                            coef( NPN_fit_bias)["Latitude"] * phen_data_of$Latitude +
                            coef( NPN_fit_bias)["Longitude"] * phen_data_of$Longitude +
                            coef( NPN_fit_bias)["Elevation_in_Meters"] * phen_data_of$Elevation_in_Meters  +
                            coef( NPN_fit)["Year"] * phen_data_of$Year +
                            coef( NPN_fit)["gs_temp"] * phen_data_of$gs_temp + 
                            coef( NPN_fit)["gs_precip"] * phen_data_of$gs_precip)
  
  
  NPN_qweibull_estimated <- mapply(qweibull, percentile, NPN_shape_param_bias, NPN_scale_param_bias)
  
  new_data[, j] <- NPN_qweibull_estimated
}

if (ni == 1) {
  column_means <- new_data
} else {
  column_means <- colMeans(new_data, na.rm = TRUE)
}

bias_results_NPN_0.5 <- data.frame(
  Observation_ID = NPN_scale_shape$Observation_ID,
  Original = NPN_scale_shape$qweibull,
  Column_Means = column_means,
  Bias = column_means - NPN_scale_shape$qweibull,
  corrected_0.5 = 2*NPN_scale_shape$qweibull-column_means, #Belitz paper
  percentile = percentile
)

ni <- 100   
samp_size <- 1
percentile <- 0.99

new_data <- matrix(0, nrow = nrow(NPN_scale_shape), ncol = ni)

NPN_scale_shape$qweibull <- mapply(qweibull, percentile, NPN_scale_shape$NPN_shape_param, NPN_scale_shape$NPN_scale_param)

for (j in 1:ni) {
  NPN_random_samples_matrix <- mapply(function(shape, scale) qweibull(runif(samp_size), shape, scale),
                                  NPN_scale_shape$NPN_shape_param, NPN_scale_shape$NPN_scale_param, SIMPLIFY = FALSE)
  
   NPN_surv_obj_bias <- Surv(as.numeric(NPN_random_samples_matrix), phen_data_of$Phenophase_Status)
   NPN_fit_bias <- flexsurvreg( NPN_surv_obj_bias ~ Latitude + Longitude + Elevation_in_Meters + Year + gs_temp + gs_precip, 
                          anc = list(shape = ~ Latitude + Longitude + Elevation_in_Meters + Year + gs_temp + gs_precip), 
                          dist = "weibull", 
                          data = phen_data_of)
  
  NPN_shape_param_bias <- exp(coef( NPN_fit_bias)["shape"] +
                            coef( NPN_fit_bias)["shape(Latitude)"] * phen_data_of$Latitude +
                            coef( NPN_fit_bias)["shape(Longitude)"] * phen_data_of$Longitude +
                            coef( NPN_fit_bias)["shape(Elevation_in_Meters)"] * phen_data_of$Elevation_in_Meters  +
                            coef( NPN_fit)["shape(Year)"] * phen_data_of$Year +
                            coef( NPN_fit)["shape(gs_temp)"] * phen_data_of$gs_temp + 
                            coef( NPN_fit)["shape(gs_precip)"] * phen_data_of$gs_precip)
  
  NPN_scale_param_bias <- exp(coef( NPN_fit_bias)["scale"] +
                            coef( NPN_fit_bias)["Latitude"] * phen_data_of$Latitude +
                            coef( NPN_fit_bias)["Longitude"] * phen_data_of$Longitude +
                            coef( NPN_fit_bias)["Elevation_in_Meters"] * phen_data_of$Elevation_in_Meters  +
                            coef( NPN_fit)["Year"] * phen_data_of$Year +
                            coef( NPN_fit)["gs_temp"] * phen_data_of$gs_temp + 
                            coef( NPN_fit)["gs_precip"] * phen_data_of$gs_precip)

  
  NPN_qweibull_estimated <- mapply(qweibull, percentile, NPN_shape_param_bias, NPN_scale_param_bias)
  
  new_data[, j] <- NPN_qweibull_estimated
}

if (ni == 1) {
  column_means <- new_data
} else {
  column_means <- colMeans(new_data, na.rm = TRUE)
}

bias_results_NPN_0.99 <- data.frame(
  Observation_ID = NPN_scale_shape$Observation_ID,
  Original = NPN_scale_shape$qweibull,
  Column_Means = column_means,
  Bias = column_means - NPN_scale_shape$qweibull,
  corrected_0.99 = 2*NPN_scale_shape$qweibull-column_means, #Belitz paper
  percentile = percentile
)


#Combine all data
bias_results_NPN <- bind_rows(bias_results_NPN_0.5, 
                              bias_results_NPN_0.01, 
                              bias_results_NPN_0.99)








