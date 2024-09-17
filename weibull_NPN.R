
library(flexsurv)
library(survival)

############estimate parameters ########################
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


scale_shape <- data.frame(phen_data_0_1$Observation_ID, shape_param, scale_param)


#############Cumulative probabilities#################

time_points <- c(min(phen_data_0_1$Day_of_Year), max(phen_data_0_1$Day_of_Year))

library(dplyr)

# Define time points
time_points <- c(min(phen_data_0_1$Day_of_Year), max(phen_data_0_1$Day_of_Year))

# Create a data frame and calculate cumulative probabilities using mutate, need rep to ensure there is a time point for each parameter
results <- data.frame(
  Time_Point = rep(time_points, length(shape_param)),
  Shape_Param = rep(shape_param, each = length(time_points)),
  Scale_Param = rep(scale_param, each = length(time_points))
) %>%
  mutate(Cumulative_Prob = pweibull(Time_Point, shape = Shape_Param, scale = Scale_Param))

print(results)


# Plot cumulative probabilities
library(ggplot2)

ggplot(results, aes(x = Time_Point, y = Cumulative_Prob)) +
  geom_line() +
  labs(title = "Cumulative Probability Function",
       x = "Time Point (Day of Year)",
       y = "Cumulative Probability") +
  theme_minimal()

################Resampling from a given weibull distribution#################
rweibull(4530, shape = shape_param, scale = scale_param)



