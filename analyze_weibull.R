library(flexsurv)
library(survival)
library(ggplot2)
library(dplyr)
library(phenesse)
library(lubridate)

#merge data
phen_subset <- phen_data[, c("Observation_ID", "Latitude", "Longitude", "Elevation_in_Meters", "Tmax", "Day_of_Year")]
analysis_data <- left_join(bias_results, phen_subset, by = "Observation_ID")

#How shape and scale changes with parameters
############## Model 1: Weibull with Covariates ##########################
fl_time_covariates <- analysis_data$Original 

############## Model 2: Weibull without Covariates ##########################
surv_obj <- Surv(phen_data$Day_of_Year, phen_data$Phenophase_Status)
fit_without_covariates <- flexsurvreg(surv_obj ~ 1,  
                                      dist = "weibull", 
                                      data = phen_data)

shape_without_covariates <- exp(coef(fit_without_covariates)["shape"])
scale_without_covariates <- exp(coef(fit_without_covariates)["scale"])

analysis_data$fl_time_no_covariates <- qweibull(0.5, shape_without_covariates, scale_without_covariates)
analysis_data$fl_time_no_covariates <- qweibull(runif(nrow(phen_data)), shape_without_covariates, scale_without_covariates)

#Plotting

ggplot(analysis_data, aes(x = Tmax)) +
  geom_line(aes(y = fl_time_no_covariates, color = "Without Covariates"), size = 0.5) +
  geom_point(aes(y = fl_time_covariates, color = "With Covariates"), size = 0.5) +
  geom_smooth(method="lm",aes(x=analysis_data$Tmax,y=fl_time_covariates)) +
  labs(x = "Maximum Temperature (Tmax)", 
       y = "Estimated Flowering Time", 
       title = "Comparison of Flowering Times: With vs. Without Covariates") +
  scale_color_manual(values = c("Without Covariates" = "blue", "With Covariates" = "red")) +
  theme_minimal()

#compare to fl_time_covariates

ggplot(analysis_data, aes(x = Tmax)) +
  geom_line(aes(y = phenesse_fl, color = "Without Covariates"), size = 0.5) +
  geom_line(aes(y = fl_time_covariates, color = "With Covariates"), size = 0.5) +
  labs(x = "Maximum Temperature (Tmax)", 
       y = "Estimated Flowering Time", 
       title = "Comparison of Flowering Times: With vs. Without Covariates") +
  scale_color_manual(values = c("Phenesse Package" = "purple", "Our Package" = "red")) +
  theme_minimal()


#Using  phenesse package
library(phenesse)
#prepping the data
# Convert Observation_Date to Date format and extract the year
phen_data <- phen_data %>%
  mutate(Observation_Date = parse_date_time(Observation_Date, orders = c("ymd"))) %>%
  mutate(year = year(Observation_Date))

phen_data_of <- phen_data %>%
  filter(Phenophase_Status == 1 & 
          grepl("Open flowers", Phenophase_Description))%>%
          group_by(Individual_ID) %>%
          sample_n(1) %>%  # Randomly select 1 row per plant
          ungroup()   

#5x5 km grid cell (Iwanycki. AN., et al.)
library(geosphere)
library(sf)
library(s2)
library(dplyr)

cell_size <- 1  #100 km
# a cell size of 1 will give on average 10 plants per cell

phen_data_of_sf <- phen_data_of %>%
  dplyr::select(c("Longitude", "Latitude"))%>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# Create a grid and assign grid locations, 
a <- st_make_grid(phen_data_of_sf, what="polygons", cellsize=cell_size)
#check lat and long of data
bbox <- st_bbox(phen_data_of_sf) #within the range 

# Plot the grid and the points
ggplot() +
  geom_sf(data = phen_data_of_sf, color = 'red', size = 0.75) + 
  geom_sf(data = a, fill = 'transparent', lwd = 0.3) +
  coord_sf(datum = NA)  +
  labs(x = "") +
  labs(y = "")

#check how many plants are in each cell (should be ~10)

plant_count_per_cell <- st_intersects(a, phen_data_of_sf) %>% #checks which points fall within each of the grid cells in a. It returns a list, where each element corresponds to a grid cell and contains the indices of the plants that fall inside that cell. 
  lapply(length) %>% #gives the number of plants per cell by counting how many plants are associated with each grid cell
  unlist()

grid_with_counts <- st_sf(a, plant_count = plant_count_per_cell) #Each polygon represents a grid cell

ggplot() +
  geom_sf(data = phen_data_of_sf, color = 'red', size = 0.75) + 
  geom_sf(data = grid_with_counts, aes(fill = plant_count), color = 'black', alpha = 0.2) +
  coord_sf(datum = NA) +
  labs(x = "") +
  labs(y = "") +
  scale_fill_viridis_c(name = "Plant Count")


#grid cell with America overlap


#group_by latitude and longitude
which(plant_count_per_cell>=10) #gives us the indices for the code below

group_1_DOY <- phen_data_of[st_intersects(a, phen_data_of_sf)[[454]],]
group_2_DOY <- phen_data_of[st_intersects(a, phen_data_of_sf)[[457]],]
group_3_DOY <- phen_data_of[st_intersects(a, phen_data_of_sf)[[509]],]
group_4_DOY <- phen_data_of[st_intersects(a, phen_data_of_sf)[[561]],]
group_5_DOY <- phen_data_of[st_intersects(a, phen_data_of_sf)[[588]],]
group_6_DOY <- phen_data_of[st_intersects(a, phen_data_of_sf)[[611]],]
group_7_DOY <- phen_data_of[st_intersects(a, phen_data_of_sf)[[560]],]


#make into usable df
combined_data <- bind_rows(
  group_1_DOY %>% dplyr::select(Observation_ID, Day_of_Year) %>% mutate(group =1),
  group_2_DOY %>% dplyr::select(Observation_ID, Day_of_Year) %>% mutate(group =2),
  group_3_DOY %>% dplyr::select(Observation_ID, Day_of_Year) %>% mutate(group =3),
  group_4_DOY %>% dplyr::select(Observation_ID, Day_of_Year) %>% mutate(group =4),
  group_5_DOY %>% dplyr::select(Observation_ID, Day_of_Year) %>% mutate(group =5),
  group_6_DOY %>% dplyr::select(Observation_ID, Day_of_Year) %>% mutate(group =6),
  group_7_DOY %>% dplyr::select(Observation_ID, Day_of_Year) %>% mutate(group =7)
  
)


#phenesse package for NPN data (Calculate phenology percentiles within each grid cell)
phenology_results <- combined_data %>%
  group_by(group) %>%
  mutate(
    percentile_50th = weib_percentile(
      observations = combined_data$Day_of_Year,
      percentile = 0.5))
# I am getting an error when I do these all together but not one by one, so the code below will do that


# Split the data by group
group_list <- split(combined_data, combined_data$group)

# Process each group and calculate the 50th percentile
results_list <- lapply(names(group_list), function(group_name) {
  group_data <- group_list[[group_name]]
  
  percentile_50th <- weib_percentile(
    observations = group_data$Day_of_Year,
    percentile = 0.5
  )
  
  # Return a data frame with the results
  data.frame(
    group = group_name,
    percentile_50th = percentile_50th
  )
})

# Combine the results into a single dataframe
phenesse_results_NPN <- bind_rows(results_list)






