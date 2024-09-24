
library(flexsurv)
library(survival)


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
#notes
require(fitdistrplus)

qweibull(0.5,shape_param[1],scale_param[1])
test <- qweibull(runif(10),shape_param[1],scale_param[1])

estimated_param <- fitdist(test,"weibull")$estimate

qweibull(0.5,estimated_param[1],estimated_param[2])
################################################################
#Comparing actual parameters to estimated (Hard coded)

# Loop through each row
for (i in 1:nrow(scale_shape)) {
  #qweibull for actual shape and scale parameters
  scale_shape$qweibull[i] <- qweibull(.5, scale_shape$shape_param[i], scale_shape$scale_param[i])
  #random samples
    random_samples <- qweibull(runif(100), scale_shape$shape_param[i], scale_shape$scale_param[i])
  scale_shape$qweibull_random[i] <- mean(random_samples)
  
#Estimate the shape and scale parameters from the random samples for estimates
  estimated_param <- fitdist(random_samples, "weibull")$estimate
  scale_shape$estimated_shape[i] <- estimated_param[1]
  scale_shape$estimated_scale[i] <- estimated_param[2]
  
#Apply qweibull for esimated
  scale_shape$qweibull_estimated[i] <- qweibull(.5, estimated_param[1], estimated_param[2])
}


scale_shape <- scale_shape %>%
  mutate(difference <- scale_shape$qweibull - mean(scale_shape$qweibull_estimated))


#Not hard coded
x <- .5 #percentile
y <- 500 #number of iterations
sample_size <- 3 # Number of rows 
#data with all rows
scale_shape <- data.frame(phen_data_0_1$Observation_ID, shape_param, scale_param) %>% 
  rename(Observation_ID = phen_data_0_1.Observation_ID )
#sample data
scale_shape_sample <- sample_n(scale_shape, sample_size)
# Loop through each row
for (i in 1:nrow(scale_shape_sample)) {
  #qweibull for actual shape and scale parameters
  scale_shape_sample$qweibull[i] <- qweibull(x, scale_shape_sample$shape_param[i], scale_shape_sample$scale_param[i])
  #random samples
  random_samples <- qweibull(runif(y), scale_shape_sample$shape_param[i], scale_shape_sample$scale_param[i])
  scale_shape_sample$qweibull_random[i] <- mean(random_samples)
  
  #Estimate the shape and scale parameters from the random samples for estimates
  estimated_param <- fitdist(random_samples, "weibull")$estimate
  scale_shape_sample$estimated_shape[i] <- estimated_param[1]
  scale_shape_sample$estimated_scale[i] <- estimated_param[2]
  
  #Apply qweibull for estimated
  scale_shape_sample$qweibull_estimated[i] <- qweibull(x, estimated_param[1], estimated_param[2])
}

#comparing the differences between actual and estimated parameters
scale_shape_sample <- scale_shape_sample %>%
  mutate(difference <- scale_shape_sample$qweibull - mean(scale_shape_sample$qweibull_estimated))
