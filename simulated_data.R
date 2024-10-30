#Simulate normally distributed data
#1. use phen_0_1 to estimate effects of temp, lat, long, elv on fl time
#2. Use to create simulated data set (normally dist)
#3. compare mean bias of estimates with the formula provided
#benchmark = simulated value

library(dplyr)
library(ggplot2)  

# Ensure that the data is not grouped, since it causes errors
phen_data <- phen_data %>% ungroup()

# Define the size of the simulation to match the original data size.
sim_size <- nrow(phen_data) 

# Calculate mean and standard deviation for each covariate we specified (Tmax, Latitude, Longitude, Elevation).
# These statistics will be parameters for generating normally distributed random values
mean_Tmax <- mean(phen_data$Tmax, na.rm = TRUE)
sd_Tmax <- sd(phen_data$Tmax, na.rm = TRUE)

mean_Latitude <- mean(phen_data$Latitude, na.rm = TRUE)
sd_Latitude <- sd(phen_data$Latitude, na.rm = TRUE)

mean_Longitude <- mean(phen_data$Longitude, na.rm = TRUE)
sd_Longitude <- sd(phen_data$Longitude, na.rm = TRUE)

mean_Elevation <- mean(phen_data$Elevation_in_Meters, na.rm = TRUE)
sd_Elevation <- sd(phen_data$Elevation_in_Meters, na.rm = TRUE)

# make the results are reproducible
set.seed(123)

# Generate a simulated dataset with normally distributed values based on the real data.
sim_data <- phen_data %>%
  mutate(
    Tmax_sim = rnorm(sim_size, mean = mean_Tmax, sd = sd_Tmax),
    
    Latitude_sim = rnorm(sim_size, mean = mean_Latitude, sd = sd_Latitude),
    
    Longitude_sim = rnorm(sim_size, mean = mean_Longitude, sd = sd_Longitude),
    
    Elevation_sim = rnorm(sim_size, mean = mean_Elevation, sd = sd_Elevation)
  )


# Simulate flowering time based on the coefficients from our original linear model
sim_data <- sim_data %>%
  mutate(
    # Calculate the simulated day of the year using the linear model formula
    Day_of_Year_sim = coefficients["(Intercept)"] +  
      coefficients["Tmax"] * Tmax_sim +              
      coefficients["Latitude"] * Latitude_sim +     
      coefficients["Longitude"] * Longitude_sim +    
      coefficients["Elevation_in_Meters"] * Elevation_sim)

# Fit a linear model to the simulated data to estimate the parameters again.
sim_fit <- lm(Day_of_Year_sim ~ Tmax_sim + Latitude_sim + Longitude_sim + Elevation_sim, 
              data = sim_data)

# Extract the coefficients from the new model fit with simulated data
simulated_coefficients <- coef(sim_fit)

# Calculate the bias 
bias_results_simulated <- data.frame(
  Benchmark = coefficients,      
  Simulated = simulated_coefficients,  
  Bias = simulated_coefficients - coefficients  # Bias = (Simulated - Original)
)

#different aproach

phen_data <- phen_data %>% ungroup()

# Extract non-biased DOY values from Weibull regression predictions
bias_doy <- bias_results$Original

# Fit a linear regression using the non-biased DOY as the response
fit <- lm(bias_doy ~ Tmax + Latitude + Longitude + Elevation_in_Meters, data = phen_data)

# Extract coefficients from the regression model
coefficients <- coef(fit)

# Define the simulation size
sim_size <- nrow(phen_data)

# Calculate means and standard deviations for covariates
mean_Tmax <- mean(phen_data$Tmax, na.rm = TRUE)
sd_Tmax <- sd(phen_data$Tmax, na.rm = TRUE)

mean_Latitude <- mean(phen_data$Latitude, na.rm = TRUE)
sd_Latitude <- sd(phen_data$Latitude, na.rm = TRUE)

mean_Longitude <- mean(phen_data$Longitude, na.rm = TRUE)
sd_Longitude <- sd(phen_data$Longitude, na.rm = TRUE)

mean_Elevation <- mean(phen_data$Elevation_in_Meters, na.rm = TRUE)
sd_Elevation <- sd(phen_data$Elevation_in_Meters, na.rm = TRUE)

# Set seed for reproducibility
set.seed(123)

# Generate simulated data using rnorm with scaled means
sim_data <- phen_data %>%
  mutate(
    Tmax_sim = rnorm(sim_size, mean = exp(coefficients["Tmax"]) * mean_Tmax, sd = sd_Tmax),
    Latitude_sim = rnorm(sim_size, mean = exp(coefficients["Latitude"]) * mean_Latitude, sd = sd_Latitude),
    Longitude_sim = rnorm(sim_size, mean = exp(coefficients["Longitude"]) * mean_Longitude, sd = sd_Longitude),
    Elevation_sim = rnorm(sim_size, mean = exp(coefficients["Elevation_in_Meters"]) * mean_Elevation, sd = sd_Elevation)
  )

# Predict new DOY using the linear model formula with simulated values
sim_data <- sim_data %>%
  mutate(
    Day_of_Year_sim = coefficients["(Intercept)"] +
      coefficients["Tmax"] * Tmax_sim +
      coefficients["Latitude"] * Latitude_sim +
      coefficients["Longitude"] * Longitude_sim +
      coefficients["Elevation_in_Meters"] * Elevation_sim
  )

# Fit a linear model to the simulated data
sim_fit <- lm(Day_of_Year_sim ~ Tmax_sim + Latitude_sim + Longitude_sim + Elevation_sim, data = sim_data)

# Extract coefficients from the new model
simulated_coefficients <- coef(sim_fit)

# Calculate bias by comparing original and simulated coefficients
bias_results_simulated <- data.frame(
  Benchmark = coefficients,
  Simulated = simulated_coefficients,
  Bias = simulated_coefficients - coefficients  # Bias = Simulated - Original
)

