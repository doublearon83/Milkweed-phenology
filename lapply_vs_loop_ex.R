
library(flexsurv)
library(survival)
require(fitdistrplus)



############ finding estimate parameters ########################
surv_obj <- Surv(phen_data_0_1$Day_of_Year, phen_data_0_1$Phenophase_Status)

# Fit the Weibull regression model
# Covariates Latitude, Longitude, and Elevation_in_Meters 
fit <- flexsurvreg(surv_obj ~ Latitude + Longitude + Elevation_in_Meters, 
                   anc = list(shape = ~ Latitude + Longitude + Elevation_in_Meters), 
                   dist = "weibull", 
                   data = phen_data_0_1)
# Display the coefficients of the model
coef(fit)

# Compute shape and scale parameters using coefficients
shape_param <- exp(coef(fit)["shape"] +
                     coef(fit)["shape(Latitude)"] * phen_data_0_1$Latitude +
                     coef(fit)["shape(Longitude)"] * phen_data_0_1$Longitude +
                     coef(fit)["shape(Elevation_in_Meters)"] * phen_data_0_1$Elevation_in_Meters)

scale_param <- exp(coef(fit)["scale"] +
                     coef(fit)["Latitude"] * phen_data_0_1$Latitude +
                     coef(fit)["Longitude"] * phen_data_0_1$Longitude +
                     coef(fit)["Elevation_in_Meters"] * phen_data_0_1$Elevation_in_Meters)



print(shape_param)
print(scale_param)


scale_shape <- data.frame(phen_data_0_1$Observation_ID, shape_param, scale_param) %>% 
  rename(Observation_ID = phen_data_0_1.Observation_ID )

##################################################################


# Loop through each row
new_data <- matrix(0,nrow=nrow(scale_shape),ncol=length(ni))
for (j in 1:ni) {
  for (i in 1:nrow(scale_shape)) {
    #qweibull for actual shape and scale parameters
    scale_shape$qweibull[i] <- qweibull(.5, scale_shape$shape_param[i], scale_shape$scale_param[i])
    #random samples
    random_samples <- qweibull(runif(100), scale_shape$shape_param[i], scale_shape$scale_param[i])
    
    #Estimate the shape and scale parameters from the random samples for estimates
    estimated_param <- fitdist(random_samples, "weibull")$estimate
    scale_shape$estimated_shape[i] <- estimated_param[1]
    scale_shape$estimated_scale[i] <- estimated_param[2]
    
    #Apply qweibull for esimated
    scale_shape$qweibull_estimated[i] <- qweibull(.5, estimated_param[1], estimated_param[2])
  }
  new_data[,j]<-scale_shape$qweibull_estimated
}


#Using lapply

#Calculate qweibull for all rows using the actual shape and scale parameters
scale_shape_test$qweibull <- mapply(qweibull, 0.5, scale_shape$shape_param, scale_shape$scale_param)

#Generate random samples for each row using a vectorized approach
random_samples_matrix <- mapply(function(shape, scale) qweibull(runif(100), shape, scale),
                                scale_shape$shape_param, scale_shape$scale_param, SIMPLIFY = FALSE)

#Estimate the Weibull distribution parameters for each set of random samples
estimated_param <- lapply(random_samples_matrix, function(samples) fitdist(samples, "weibull")$estimate)

#Extract the estimated shape and scale parameters into separate columns
scale_shape$estimated_shape[i] <- estimated_param[1]
scale_shape$estimated_scale[i] <- estimated_param[2]

#Apply qweibull for the estimated parameters using the vectorized function
scale_shape_test$qweibull_estimated <- mapply(qweibull, 0.5, scale_shape$estimated_shape, scale_shape$estimated_scale)


# View the final data frame
print(scale_shape)