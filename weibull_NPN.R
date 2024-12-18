
library(flexsurv)
library(survival)
require(fitdistrplus)
library(dplyr)

############ finding estimate parameters ########################
phen_data_of <- phen_data %>%
  filter(phen_data$Phenophase_Status== 1 & phen_data$Phenophase_Description == "Open flowers")

surv_obj <- Surv(phen_data_of$Day_of_Year, phen_data_of$Phenophase_Status)

# Fit the Weibull regression model
# Covariates Latitude, Longitude, and Elevation_in_Meters 
  fit <- flexsurvreg(surv_obj ~ Latitude + Longitude + Elevation_in_Meters, 
                   anc = list(shape = ~ Latitude + Longitude + Elevation_in_Meters), 
                   dist = "weibull", 
                   data = phen_data_of)
# Display the coefficients of the model
coef(fit)

# Compute shape and scale parameters using coefficients
shape_param <- exp(coef(fit)["shape"] +
                     coef(fit)["shape(Latitude)"] * phen_data$Latitude +
                     coef(fit)["shape(Longitude)"] * phen_data$Longitude +
                     coef(fit)["shape(Elevation_in_Meters)"] * phen_data$Elevation_in_Meters)

scale_param <- exp(coef(fit)["scale"] +
                     coef(fit)["Latitude"] * phen_data$Latitude +
                     coef(fit)["Longitude"] * phen_data$Longitude +
                     coef(fit)["Elevation_in_Meters"] * phen_data$Elevation_in_Meters)



print(shape_param)
print(scale_param)


scale_shape <- data.frame(phen_data$Observation_ID, shape_param, scale_param) %>% 
  rename(Observation_ID = phen_data.Observation_ID )

##################################################################

# Define the number of iterations
ni <- 1  # Set the number of iterations 
samp_size <- 25
percentile <- 0.5

# Initialize a matrix to store estimated qweibull values for each iteration
new_data <- matrix(0, nrow = nrow(scale_shape), ncol = ni)

# Calculate qweibull for all rows using the actual shape and scale parameters
scale_shape$qweibull <- mapply(qweibull, percentile, scale_shape$shape_param, scale_shape$scale_param)

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
  scale_shape$qweibull_estimated <- mapply(qweibull, percentile, scale_shape$estimated_shape, scale_shape$estimated_scale)
  
  # Store the estimated qweibull values for this iteration in the matrix
  new_data[, j] <- scale_shape$qweibull_estimated
}


# Calculate the mean for each column
if (ni == 1) {
  column_means <- new_data
} else {
  column_means <- colMeans(new_data, na.rm = TRUE)
}

bias_results_NPN <- data.frame(
  Observation_ID = scale_shape$Observation_ID,
  Original = scale_shape$qweibull,
  Column_Means = column_means,
  Bias = column_means - scale_shape$qweibull,
  corrected = 2*scale_shape$qweibull-column_means #Belitz paper
)

bias_results_NPN









