### Met and Herbarium data combined, Weibull

#packages
library(tidyr)
library(flexsurv)
library(survival)
require(fitdistrplus)
library(dplyr)

#Data sets
HerbariumData_fl #from herbarium_wrangling
combined_df_selected2 #from NOAA_met_data.r

analyze_df <- inner_join(HerbariumData_fl, combined_df_selected2, by = c("Observation_ID" = "plantid"))

###### Weibull ######

############ finding estimate parameters ########################
H_surv_obj <- Surv(analyze_df$Day_of_Year, analyze_df$Phenophase_Status)

# H_fit the Weibull regression model
# Covariates Latitude, Longitude, and Elevation_in_Meters 
H_fit <- flexsurvreg(H_surv_obj ~ Latitude + Longitude + Elevation_in_Meters + Year + mean_tmax + mean_prcp, 
                   anc = list(shape = ~ Latitude + Longitude + Elevation_in_Meters  + Year + mean_tmax + mean_prcp ), 
                   dist = "weibull", 
                   data = analyze_df)
# Display the coefficients of the model
coef(H_fit)

# Compute shape and scale parameters using coefficients
H_shape_param <- exp(coef(H_fit)["shape"] +
                            coef(H_fit)["shape(Latitude)"] * analyze_df$Latitude +
                            coef(H_fit)["shape(Longitude)"] * analyze_df$Longitude +
                            coef(H_fit)["shape(Elevation_in_Meters)"] * analyze_df$Elevation_in_Meters +
                       coef(H_fit)["shape(Year)"] * analyze_df$Year +
                       coef(H_fit)["shape(mean_tmax)"] * analyze_df$mean_tmax +
                       coef(H_fit)["shape(mean_prcp)"] * analyze_df$mean_prcp
                     )

H_scale_param <- exp(coef(H_fit)["scale"] +
                            coef(H_fit)["Latitude"] * analyze_df$Latitude +
                            coef(H_fit)["Longitude"] * analyze_df$Longitude +
                            coef(H_fit)["Elevation_in_Meters"] * analyze_df$Elevation_in_Meters + 
                       coef(H_fit)["Year"] * analyze_df$Year +
                       coef(H_fit)["mean_tmax"] * analyze_df$mean_tmax +
                       coef(H_fit)["shape(mean_prcp)"] * analyze_df$mean_prcp
            )

H_scale_shape <- data.frame(analyze_df$Observation_ID, H_shape_param, H_scale_param) %>% 
  rename(Observation_ID = analyze_df.Observation_ID )

##################################################################
############Run through for all percentiles (hard coded)##################

ni <- 100  # Set the number of iterations 
samp_size <- 1
percentile <- 0.01

new_data <- matrix(0, nrow = nrow(H_scale_shape), ncol = ni)

H_scale_shape$qweibull <- mapply(qweibull, percentile, H_scale_shape$H_shape_param, H_scale_shape$H_scale_param)

for (j in 1:ni) {
  H_random_samples_matrix <- mapply(function(shape, scale) qweibull(runif(samp_size), shape, scale),
                                  H_scale_shape$H_shape_param, H_scale_shape$H_scale_param, SIMPLIFY = FALSE)
  
  H_surv_obj_bias <- Surv(as.numeric(H_random_samples_matrix), analyze_df$Phenophase_Status)
  
  H_fit_bias <- flexsurvreg(H_surv_obj_bias ~ Latitude + Longitude + Elevation_in_Meters + Year + mean_tmax + mean_prcp , 
                          anc = list(shape = ~ Latitude + Longitude + Elevation_in_Meters + Year + mean_tmax + mean_prcp  ), 
                          dist = "weibull", 
                          data = analyze_df)
  
  H_shape_param_bias <- exp(coef(H_fit_bias)["shape"] +
                            coef(H_fit_bias)["shape(Latitude)"] * analyze_df$Latitude +
                            coef(H_fit_bias)["shape(Longitude)"] * analyze_df$Longitude +
                            coef(H_fit_bias)["shape(Elevation_in_Meters)"] * analyze_df$Elevation_in_Meters +
                              coef(H_fit_bias)["shape(Year)"] * analyze_df$Year   +
                              coef(H_fit_bias)["shape(mean_tmax)"] * analyze_df$mean_tmax  +
                              coef(H_fit_bias)["shape(mean_prcp)"] * analyze_df$mean_prcp
                          )
  
  H_scale_param_bias <- exp(coef(H_fit_bias)["scale"] +
                            coef(H_fit_bias)["Latitude"] * analyze_df$Latitude +
                            coef(H_fit_bias)["Longitude"] * analyze_df$Longitude +
                            coef(H_fit_bias)["Elevation_in_Meters"] * analyze_df$Elevation_in_Meters +
                              coef(H_fit_bias)["Year"] * analyze_df$Year  +
                              coef(H_fit_bias)["mean_tmax"] * analyze_df$mean_tmax +
                              coef(H_fit_bias)["mean_prcp"] * analyze_df$mean_prcp
                              )
  
  H_qweibull_estimated <- mapply(qweibull, percentile, H_shape_param_bias, H_scale_param_bias)
  
  new_data[, j] <- H_qweibull_estimated
}

if (ni == 1) {
  column_means <- as.numeric(new_data)
} else {
  column_means <- rowMeans(new_data, na.rm = TRUE)
}

bias_results_herbarium_0.01 <- data.frame(
  Observation_ID = H_scale_shape$Observation_ID,
  Original = H_scale_shape$qweibull,
  Column_Means = column_means,
  Bias = column_means - H_scale_shape$qweibull,
  corrected_0.01 = 2*H_scale_shape$qweibull-column_means, #Belitz paper
  percentile = percentile
)



ni <- 100  # Set the number of iterations 
samp_size <- 1
percentile <- 0.5

new_data <- matrix(0, nrow = nrow(H_scale_shape), ncol = ni)

H_scale_shape$qweibull <- mapply(qweibull, percentile, H_scale_shape$H_shape_param, H_scale_shape$H_scale_param)

for (j in 1:ni) {
  H_random_samples_matrix <- mapply(function(shape, scale) qweibull(runif(samp_size), shape, scale),
                                  H_scale_shape$H_shape_param, H_scale_shape$H_scale_param, SIMPLIFY = FALSE)
  
  H_surv_obj_bias <- Surv(as.numeric(H_random_samples_matrix), analyze_df$Phenophase_Status)
  
  H_fit_bias <- flexsurvreg(H_surv_obj_bias ~ Latitude + Longitude + Elevation_in_Meters + Year + mean_tmax, 
                          anc = list(shape = ~ Latitude + Longitude + Elevation_in_Meters + Year + mean_tmax ), 
                          dist = "weibull", 
                          data = analyze_df)
  
  
  H_shape_param_bias <- exp(coef(H_fit_bias)["shape"] +
                              coef(H_fit_bias)["shape(Latitude)"] * analyze_df$Latitude +
                              coef(H_fit_bias)["shape(Longitude)"] * analyze_df$Longitude +
                              coef(H_fit_bias)["shape(Elevation_in_Meters)"] * analyze_df$Elevation_in_Meters +
                              coef(H_fit_bias)["shape(Year)"] * analyze_df$Year   +
                              coef(H_fit_bias)["shape(mean_tmax)"] * analyze_df$mean_tmax  +
                              coef(H_fit_bias)["shape(mean_prcp)"] * analyze_df$mean_prcp
  )
  
  H_scale_param_bias <- exp(coef(H_fit_bias)["scale"] +
                              coef(H_fit_bias)["Latitude"] * analyze_df$Latitude +
                              coef(H_fit_bias)["Longitude"] * analyze_df$Longitude +
                              coef(H_fit_bias)["Elevation_in_Meters"] * analyze_df$Elevation_in_Meters +
                              coef(H_fit_bias)["Year"] * analyze_df$Year  +
                              coef(H_fit_bias)["mean_tmax"] * analyze_df$mean_tmax +
                              coef(H_fit_bias)["mean_prcp"] * analyze_df$mean_prcp
  )
  
  H_qweibull_estimated <- mapply(qweibull, percentile, H_shape_param_bias, H_scale_param_bias)
  
  new_data[, j] <- H_qweibull_estimated
}

if (ni == 1) {
  column_means <- as.numeric(new_data)
} else {
  column_means <- rowMeans(new_data, na.rm = TRUE)
}

bias_results_herbarium_0.5 <- data.frame(
  Observation_ID = H_scale_shape$Observation_ID,
  Original = H_scale_shape$qweibull,
  Column_Means = column_means,
  Bias = column_means - H_scale_shape$qweibull,
  corrected_0.5 = 2*H_scale_shape$qweibull-column_means, #Belitz paper
  percentile = percentile
)

ni <- 100   
samp_size <- 1
percentile <- 0.99

new_data <- matrix(0, nrow = nrow(H_scale_shape), ncol = ni)

H_scale_shape$qweibull <- mapply(qweibull, percentile, H_scale_shape$H_shape_param, H_scale_shape$H_scale_param)

for (j in 1:ni) {
  H_random_samples_matrix <- mapply(function(shape, scale) qweibull(runif(samp_size), shape, scale),
                                  H_scale_shape$H_shape_param, H_scale_shape$H_scale_param, SIMPLIFY = FALSE)
  
  H_surv_obj_bias <- Surv(as.numeric(H_random_samples_matrix), analyze_df$Phenophase_Status)
  H_fit_bias <- flexsurvreg(H_surv_obj_bias ~ Latitude + Longitude + Elevation_in_Meters + Year + mean_tmax, 
                          anc = list(shape = ~ Latitude + Longitude + Elevation_in_Meters + Year + mean_tmax  ), 
                          dist = "weibull", 
                          data = analyze_df)
  
  H_shape_param_bias <- exp(coef(H_fit_bias)["shape"] +
                              coef(H_fit_bias)["shape(Latitude)"] * analyze_df$Latitude +
                              coef(H_fit_bias)["shape(Longitude)"] * analyze_df$Longitude +
                              coef(H_fit_bias)["shape(Elevation_in_Meters)"] * analyze_df$Elevation_in_Meters +
                              coef(H_fit_bias)["shape(Year)"] * analyze_df$Year   +
                              coef(H_fit_bias)["shape(mean_tmax)"] * analyze_df$mean_tmax  +
                              coef(H_fit_bias)["shape(mean_prcp)"] * analyze_df$mean_prcp
  )
  
  H_scale_param_bias <- exp(coef(H_fit_bias)["scale"] +
                              coef(H_fit_bias)["Latitude"] * analyze_df$Latitude +
                              coef(H_fit_bias)["Longitude"] * analyze_df$Longitude +
                              coef(H_fit_bias)["Elevation_in_Meters"] * analyze_df$Elevation_in_Meters +
                              coef(H_fit_bias)["Year"] * analyze_df$Year  +
                              coef(H_fit_bias)["mean_tmax"] * analyze_df$mean_tmax +
                              coef(H_fit_bias)["mean_prcp"] * analyze_df$mean_prcp
  )
  
  H_qweibull_estimated <- mapply(qweibull, percentile, H_shape_param_bias, H_scale_param_bias)
  
  new_data[, j] <- H_qweibull_estimated
}

if (ni == 1) {
  column_means <- as.numeric(new_data)
} else {
  column_means <- rowMeans(new_data, na.rm = TRUE)
}

bias_results_herbarium_0.99 <- data.frame(
  Observation_ID = H_scale_shape$Observation_ID,
  Original = H_scale_shape$qweibull,
  Column_Means = column_means,
  Bias = column_means - H_scale_shape$qweibull,
  corrected_0.99 = 2*H_scale_shape$qweibull-column_means, #Belitz paper
  percentile = percentile
)


#Combine all data
bias_results_herbarium <- bind_rows(bias_results_herbarium_0.5, 
                              bias_results_herbarium_0.01, 
                              bias_results_herbarium_0.99)








