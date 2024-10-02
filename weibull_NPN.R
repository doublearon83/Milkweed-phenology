
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
#notes
require(fitdistrplus)

qweibull(0.5,shape_param[1],scale_param[1])
test <- qweibull(runif(10),shape_param[1],scale_param[1])

estimated_param <- fitdist(test,"weibull")$estimate

qweibull(0.5,estimated_param[1],estimated_param[2])
################################################################
#Comparing actual parameters to estimated (Hard coded)

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



#using loops for all estimations
# Define the number of iterations
ni <- 5  # Set the number of iterations

# Initialize a matrix to store estimated qweibull values for each iteration
new_data <- matrix(0, nrow = nrow(scale_shape), ncol = ni)

# Loop through each iteration
for (j in 1:ni) {
  for (i in 1:nrow(scale_shape)) {
    # qweibull for actual shape and scale parameters (baseline calculation)
    scale_shape$qweibull[i] <- qweibull(.5, scale_shape$shape_param[i], scale_shape$scale_param[i])
    
    # Generate random samples using the actual shape and scale parameters
    random_samples <- qweibull(runif(100), scale_shape$shape_param[i], scale_shape$scale_param[i])
    
    # Estimate the shape and scale parameters from the random samples for estimates
    estimated_param <- fitdist(random_samples, "weibull")$estimate
    scale_shape$estimated_shape[i] <- estimated_param[1]
    scale_shape$estimated_scale[i] <- estimated_param[2]
    
    # Apply qweibull using the estimated parameters
    scale_shape$qweibull_estimated[i] <- qweibull(.5, estimated_param[1], estimated_param[2])
  }
  # Store the estimated qweibull values for this iteration in the matrix
  new_data[, j] <- scale_shape$qweibull_estimated
}

# Calculate the mean for each column
column_means <- colMeans(new_data)

bias_result <- data.frame(
  Original = scale_shape$qweibull,
  Column_Means = column_means,
  Bias = column_means - scale_shape$qweibull  
)



##using vectorization for all estimates
# Define the number of iterations
ni <- 1  # Set the number of iterations 
samp_size <- 25

# Initialize a matrix to store estimated qweibull values for each iteration
new_data <- matrix(0, nrow = nrow(scale_shape), ncol = ni)

# Calculate qweibull for all rows using the actual shape and scale parameters
scale_shape$qweibull <- mapply(qweibull, 0.5, scale_shape$shape_param, scale_shape$scale_param)

# Loop through each iteration
for (j in 1:ni) {
  # Generate random samples for each row using a vectorized approach
  random_samples_matrix <- mapply(function(shape, scale) qweibull(runif(samp_size), shape, scale),
                                  scale_shape$shape_param, scale_shape$scale_param, SIMPLIFY = FALSE)
  
  # Estimate the Weibull distribution parameters for each set of random samples
  estimated_param <- lapply(random_samples_matrix, function(samples) fitdist(samples, "weibull")$estimate)
  
  # Extract the estimated shape and scale parameters into separate columns
  scale_shape$estimated_shape <- sapply(estimated_param, `[[`, 1)
  scale_shape$estimated_scale <- sapply(estimated_param, `[[`, 2)
  # Apply qweibull for the estimated parameters using the vectorized function
  scale_shape$qweibull_estimated <- mapply(qweibull, 0.5, scale_shape$estimated_shape, scale_shape$estimated_scale)
  
  # Store the estimated qweibull values for this iteration in the matrix
  new_data[, j] <- scale_shape$qweibull_estimated
}


# Calculate the mean for each column
if (ni==1) {column_means<-new_data} else {column_means <- colMeans(new_data)}

bias_results <- data.frame(
  Original = scale_shape$qweibull,
  Column_Means = column_means,
  Bias = column_means - scale_shape$qweibull,
  corrected = 2*scale_shape$qweibull-column_means
)















#Not hard coded
x <- .5 #percentile
y <- 500 #sample size
scale_shape <- data.frame(phen_data_0_1$Observation_ID, shape_param, scale_param) %>% 
  rename(Observation_ID = phen_data_0_1.Observation_ID )
#sample data
# Loop through each row
for (i in 1:nrow(scale_shape_sample)) {
  #qweibull for actual shape and scale parameters
  scale_shape_sample$qweibull[i] <- qweibull(x, scale_shape_sample$shape_param[i], scale_shape_sample$scale_param[i])
  #random samples
  random_samples <- qweibull(runif(y), scale_shape_sample$shape_param[i], scale_shape_sample$scale_param[i])

  
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
