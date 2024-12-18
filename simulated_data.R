#Simulate normally distributed data
#1. use phen_0_1 to estimate effects of temp, lat, long, elv on fl time
#2. Use to create simulated data set (normally dist)
#3. compare mean bias of estimates with the formula provided
#benchmark = simulated value

library(dplyr)
library(ggplot2)  

# Create a new column Tmax_growing by averaging Tmax_spring, Tmax_summer, and Tmax_fall
phen_data <- phen_data %>%
  mutate(Tmax_growing = rowMeans(across(c(Tmax_Spring, Tmax_Summer, Tmax_Fall)), na.rm = TRUE))

# Extract non-biased DOY values from Weibull regression predictions
bias_doy <- bias_results$Original

# Fit a linear regression using the non-biased DOY as the response
fit <- lm(bias_doy ~ Tmax_growing + Latitude + Longitude + Elevation_in_Meters, data = phen_data)

# Extract coefficients from the regression model
coefficients <- coef(fit)

# Define the simulation size
sim_size <- nrow(phen_data)

#sigma
sigma <- summary(fit)$sigma

# Calculate means and standard deviations for covariates
sim_data <- phen_data %>%
  mutate(
    Tmax_sim = rnorm(sim_size, mean(Tmax_growing, na.rm = TRUE), sd = sd(phen_data$Tmax_growing, na.rm = TRUE)),
    Latitude_sim = rnorm(sim_size, mean(Latitude, na.rm = TRUE), sd = sd(phen_data$Latitude, na.rm = TRUE)),
    Longitude_sim = rnorm(sim_size, mean(Longitude, na.rm = TRUE), sd = sd(phen_data$Longitude, na.rm = TRUE)),
    Elevation_sim = rnorm(sim_size, mean(Elevation_in_Meters, na.rm = TRUE), sd = sd(phen_data$Elevation_in_Meters, na.rm = TRUE)),
    sigma_sim = rnorm(sim_size,0,sigma)
  )

# Set seed for reproducibility
set.seed(123)

# Calculate simulated Day of Year (DOY) using the linear model formula
sim_data <- sim_data %>%
  mutate(
    Day_of_Year_sim = coefficients["(Intercept)"] +
      coefficients["Tmax_growing"] * Tmax_sim +
      coefficients["Latitude"] * Latitude_sim +
      coefficients["Longitude"] * Longitude_sim +
      coefficients["Elevation_in_Meters"] * Elevation_sim +
      sigma_sim
  )


# simulate phenophase status
sim_data <- sim_data %>%
  mutate(Phenophase_Status_sim = rbinom(sim_size, 1, 0.5))  # 50% probability of success

# Create the results data frame
sim_results <- data.frame(
  Observation_ID = phen_data$Observation_ID, 
  Original = bias_doy,
  Simulated = sim_data$Day_of_Year_sim,
  Latitude = sim_data$Latitude_sim,
  Longitude = sim_data$Longitude_sim,
  Elevation = sim_data$Elevation_sim,
  Phenophase_Status = sim_data$Phenophase_Status_sim
)


#grid and using phenesse
cell_size <- 1  #100 km
# a cell size of 1 will give on average 10 plants per cell

phen_data_simulated_sf <- sim_results %>%
  select(c("Longitude", "Latitude"))%>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# Create a grid and assign grid locations, 
b <- st_make_grid(phen_data_simulated_sf, what="polygons", cellsize=cell_size)

# Plot the grid and the points
ggplot() +
  geom_sf(data = phen_data_simulated_sf, color = 'red', size = 0.75) + 
  geom_sf(data = b, fill = 'transparent', lwd = 0.3) +
  coord_sf(datum = NA)  +
  labs(x = "") +
  labs(y = "")

plant_count_per_cell_sim <- st_intersects(b, phen_data_simulated_sf) %>%
  lapply(length) %>% 
  unlist()

grid_with_counts_sim <- st_sf(b, plant_count = plant_count_per_cell_sim) 

ggplot() +
  geom_sf(data = phen_data_simulated_sf, color = 'red', size = 0.75) + 
  geom_sf(data = grid_with_counts_sim, aes(fill = plant_count), color = 'black', alpha = 0.2) +
  coord_sf(datum = NA) +
  labs(x = "") +
  labs(y = "") +
  scale_fill_viridis_c(name = "Plant Count")

#change to fit our data for phenesse
group_DOY_list_sim <- list()

# Loop through grid cell indices
for (i in seq_along(grid_cell_indices)) {
  index <- grid_cell_indices[i]
  intersected_data <- phen_data_of[st_intersects(b, phen_data_simulated_sf)[[index]], ]

  # Check if there are more than 10 flowers
  if (nrow(intersected_data) >= 1000) {
    group_DOY_list_sim[[i]] <- intersected_data %>%
      dplyr::select(Observation_ID, Day_of_Year) %>%
      mutate(group = i)
  }
}


#using lapply
grid_cell_indices_sim <- which(plant_count_per_cell_sim>=1000) #gives us the indices for the code below

group_DOY_list_sim <- lapply(seq_along(grid_cell_indices), function(i) {
  index <- grid_cell_indices[i]
  intersected_data <- phen_data_of[st_intersects(b, phen_data_simulated_sf)[[index]], ]
  intersected_data %>%
    dplyr::select(Observation_ID, Day_of_Year) %>%
    mutate(group = i)
})

combined_data_sim <- dplyr::bind_rows(group_DOY_list_sim)


#lapply and looping
grid_cell_indices_sim <- which(plant_count_per_cell_sim >= 1000)

group_DOY_list_sim <- lapply(seq_along(grid_cell_indices_sim), function(i) {

  index <- grid_cell_indices_sim[i]
  
  intersected_data <- phen_data_of[st_intersects(b, phen_data_simulated_sf)[[index]], ]
  
  intersected_data %>%
    dplyr::select(Observation_ID, Day_of_Year) %>%
    mutate(group = index)  
})

# Combine the list of data frames into one data frame
combined_data_sim <- dplyr::bind_rows(group_DOY_list_sim)



#phenesse package for simulated data (Calculate phenology percentiles within each grid cell)
phenesse_results_sim <- combined_data_sim %>%
  group_by(group) %>%
  mutate(
    percentile_50th = weib_percentile(
      observations = Day_of_Year,
      percentile = 0.5))


#once again, can calculate one-by-one, but not with a group of data 
# Split data by group, issues with NA, so filtered for them
group_list_sim <- split(combined_data_sim, combined_data_sim$group)

results_list_sim <- lapply(names(group_list_sim), function(group_name) {
  group_data_sim <- group_list_sim[[group_name]]
  
  # Filter out rows with NA or non-positive Day_of_Year values
  group_data_sim_clean <- group_data_sim %>%
    filter(!is.na(Day_of_Year) & Day_of_Year > 0)
  
  if (nrow(group_data_sim_clean) < 2) {
    return(data.frame(group = group_name, percentile_50th = NA)) 
  }
  
  percentile_50th <- weib_percentile(
    observations = group_data_sim_clean$Day_of_Year,
    percentile = 0.5
  )
  
  data.frame(
    group = group_name,
    percentile_50th = percentile_50th
  )
})

phenesse_results_sim <- bind_rows(results_list_sim)


#Weibull distribution of simulated data

############ finding estimate parameters ########################
surv_obj_sim <- Surv(sim_results$Original, sim_results$Phenophase_Status)


# Fit the Weibull regression model
# Covariates Latitude, Longitude, and Elevation_in_Meters 
fit_sim <- flexsurvreg(surv_obj_sim ~ Latitude + Longitude + Elevation_in_Meters, 
                   anc = list(shape = ~ Latitude + Longitude + Elevation_in_Meters), 
                   dist = "weibull", 
                   data = phen_data)
# Display the coefficients of the model
coef(fit_sim)

# Compute shape and scale parameters using the coefficients from the fitted model
shape_param_sim <- exp(coef(fit_sim)["shape"] +
                         coef(fit_sim)["shape(Latitude)"] * sim_results$Latitude +
                         coef(fit_sim)["shape(Longitude)"] * sim_results$Longitude +
                         coef(fit_sim)["shape(Elevation_in_Meters)"] * sim_results$Elevation)

scale_param_sim <- exp(coef(fit_sim)["scale"] +
                         coef(fit_sim)["Latitude"] * sim_results$Latitude +
                         coef(fit_sim)["Longitude"] * sim_results$Longitude +
                         coef(fit_sim)["Elevation_in_Meters"] * sim_results$Elevation)

# Check the output
print(shape_param_sim)
print(scale_param_sim)



scale_shape_sim <- data.frame(sim_results$Observation_ID, shape_param_sim, scale_param_sim) %>% 
  rename(Observation_ID = sim_results.Observation_ID )

##################################################################
#error: failed to estimate the parameters with error code 1
ni <- 1  
samp_size <- 25
percentile <- 0.5

new_data <- matrix(0, nrow = nrow(scale_shape_sim), ncol = ni)

scale_shape_sim$qweibull <- mapply(qweibull, percentile, scale_shape_sim$shape_param_sim, scale_shape_sim$scale_param_sim)

for (j in 1:ni) {
  random_samples_matrix <- mapply(function(shape, scale) qweibull(runif(samp_size), shape, scale),
                                  scale_shape_sim$shape_param_sim, scale_shape_sim$scale_param_sim, SIMPLIFY = FALSE)
  
  estimated_param <- lapply(random_samples_matrix, function(samples) fitdist(samples, "weibull")$estimate)
  
  scale_shape_sim$estimated_shape <- sapply(estimated_param, `[[`, 1)
  scale_shape_sim$estimated_scale <- sapply(estimated_param, `[[`, 2)
  
  scale_shape_sim$qweibull_estimated <- mapply(qweibull, percentile, scale_shape_sim$estimated_shape, scale_shape_sim$estimated_scale)
  
  new_data[, j] <- scale_shape_sim$qweibull_estimated
}


if (ni == 1) { 
  column_means <- new_data 
} else { 
  column_means <- colMeans(new_data) 
}

# Calculate the bias and corrected values
bias_results_sim <- data.frame(
  Observation_ID = scale_shape_sim$Observation_ID,
  Original = scale_shape_sim$qweibull,
  Column_Means = column_means,
  Bias = column_means - scale_shape_sim$qweibull, 
  Corrected = 2 * scale_shape_sim$qweibull - column_means #Betlitz paper
)

bias_results_sim


#compare all 4 tests 
bias_results_sim
bias_results_NPN
phenesse_results_NPN
phenesse_results_sim

