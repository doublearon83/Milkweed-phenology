####Extracting meteorological data from NOAA

#packages
library(tidyverse)
require(devtools)
#devtools::install_github("ropensci/rnoaa")#for downloading rnoaa from github
library(rnoaa)


status_intensity_observation <- read.csv("status_intensity_observation_data.csv", header = TRUE)

#Creating plant_data_id
plant_id <- unique(status_intensity_observation$Individual_ID)
latitude <- status_intensity_observation$Latitude[match(plant_id,status_intensity_observation$Individual_ID)]
longitude <- status_intensity_observation$Longitude[match(plant_id,status_intensity_observation$Individual_ID)]
plant_data_id <- data.frame(plant_id,latitude,longitude)

names(plant_data_id)[1] <- "id"

#subsetting phenology dataset
status_intensity_observation_sub  <- status_intensity_observation[,c(4,5,13,15,16,18)]

########### Batch Running Code to find (1st) nearest stations ############
# Initialize ONCE, 
#batch_results_df <- data.frame()
batch_results_df <- read.csv("nearest_stations.csv")

# Define the range of rows, next start with 647
start_row <- 1
end_row <- 646

system.time({
  # Iterate through the specified range of rows
  for (i in start_row:end_row) {
    # Extract the plant_id, latitude, longitude, and year
    plant_id <- HerbariumData_nona$Observation_ID[i]
    latitude <- HerbariumData_nona$Latitude[i]
    longitude <- HerbariumData_nona$Longitude[i]
    year <- HerbariumData_nona$Year[i]
    
    # Create a temporary data frame to follow mete_nearby_stations function format (necessary!)
    temp_df <- data.frame(id = plant_id, latitude = latitude, longitude = longitude)
    
    for (Year in 1840:2024) {
      
      if (year!=Year) {next} else {   
    # Call the function for the specific year
    stations <- meteo_nearby_stations(lat_lon_df = temp_df,
                                      lat_colname = "latitude",
                                      lon_colname = "longitude",
                                      var = "TMAX",
                                      year_min = year,
                                      year_max = year,
                                      limit = 1)
    
    # Extract the station name and distance from the first station in the list
    
    station_info <- stations[[1]]
    station_name <- station_info$name[1]
    station_distance <- station_info$distance[1]
    station_id <- station_info$id[1]
    
    # Create a new row for the results
    new_row <- data.frame(id = station_id,
                          PlantID = plant_id,
                          Latitude = latitude,
                          Longitude = longitude,
                          Year = year,
                          StationName = station_name,
                          Distance = station_distance)
    
    # Append data to the batch data frame
    batch_results_df <- rbind(batch_results_df, new_row)
      }}  
  }
})

# Save the batch results to an existing CSV file
output_file <- '/Users/sarah/OneDrive - Franklin & Marshall College/Documents/GitHub/Milkweed-phenology/nearest_stations_.csv'
write.csv(batch_results_df, file = output_file, row.names = FALSE)

#####################################################
#####################################################
#uploading stations data
stations <- read.csv("stations2_", header = TRUE)

#uploading stat_plant_data
stat_plant <- read.csv("stat_plant", header = TRUE)

############pull met using a loop ##################
# Initialize ONCE, 
#met_df <- data.frame()
met_df <- read.csv("met_data.csv")
stations2 <- read.csv("nearest_stations_.csv")

#start with 637 next
start_row <- 1
end_row <- 646

system.time({
  # Iterate through the specified range of rows
  for (i in start_row:end_row) {

    station_id <- stations2$id[i]
    year <- stations2$Year[i]
    
    for (Year in 1840:2024) {
      
      if (year!=Year) {next} else {   
    
        met_data <- meteo_pull_monitors(unique(station_id),
                            date_min = paste0(Year, "-03-01"),
                            date_max = paste0(Year, "-11-30"),
                            var = c("TMAX", "PRCP"))
        
        if (nrow(met_data) == 0 | !"tmax" %in% colnames(met_data) |  !"prcp" %in% colnames(met_data)) {
          id <- stations2$id[i]
          date <- NA
          prcp <- NA
          tmax <- NA
          plantid <- stations2$PlantID[i]
        }
        else {
          id <- met_data$id 
          date <- met_data$date
          prcp <- met_data$prcp
          tmax <- met_data$tmax
          plantid <- rep(stations2$PlantID[i],length(id))
        }
        
        
        # Create a new row for the results
        new_r <- data.frame(id = id,
                            date = date,
                            prcp = prcp,
                            tmax = tmax,
                            Year = Year,
                            plantid = plantid)
        
        # Append data to the batch data frame
        met_df <- rbind(met_df, new_r)
      }}
  }
})

# Save the batch results to an existing CSV file
output_file <- '/Users/sarah/OneDrive - Franklin & Marshall College/Documents/GitHub/Milkweed-phenology/met_data_.csv'
write.csv(met_df, file = output_file, row.names = FALSE)

###########################################################
#To find a second closest station if the first has NA values
#create a subset
na_subset <- met_df %>%
  filter(is.na(prcp) | is.na(tmax)) %>%
  dplyr::select(plantid) %>%
  distinct() %>%
  inner_join(HerbariumData_nona, by = join_by(plantid == Observation_ID))

# Initialize ONCE, 
#na_df <- data.frame()
na_df <- read.csv("nearest_stations2_.csv")

#check number of rows
nrow(na_subset)
# Define the range of rows, next start with 359
start_row <- 1
end_row <- 358

system.time({
  # Iterate through the specified range of rows
  for (i in start_row:end_row) {
    # Extract the plant_id, latitude, longitude, and year
    plant_id <- na_subset$plantid[i]
    latitude <- na_subset$Latitude[i]
    longitude <- na_subset$Longitude[i]
    year <- na_subset$Year[i]
    
    # Create a temporary data frame to follow mete_nearby_stations function format (necessary!)
    temp_df <- data.frame(id = plant_id, latitude = latitude, longitude = longitude)
    
    for (Year in 1840:2024) {
      
      if (year!=Year) {next} else {   
        # Call the function for the specific year
        stations <- meteo_nearby_stations(lat_lon_df = temp_df,
                                          lat_colname = "latitude",
                                          lon_colname = "longitude",
                                          var = "TMAX",
                                          year_min = year,
                                          year_max = year,
                                          limit = 2)
        
        # Extract the station name and distance from the first station in the list
        
        station_info <- stations[[1]][2,]
        station_name <- station_info$name[1]
        station_distance <- station_info$distance[1]
        station_id <- station_info$id[1]
        
        # Create a new row for the results
        new_row <- data.frame(id = station_id,
                              PlantID = plant_id,
                              Latitude = latitude,
                              Longitude = longitude,
                              Year = year,
                              StationName = station_name,
                              Distance = station_distance)
        
        # Append data to the batch data frame
        na_df <- rbind(na_df, new_row)
      }}  
  }
})

# Save the batch results to an existing CSV file
output_file <- '/Users/sarah/OneDrive - Franklin & Marshall College/Documents/GitHub/Milkweed-phenology/nearest_stations2_.csv'
write.csv(na_df, file = output_file, row.names = FALSE)

###find the met data for second closest station####

# Initialize ONCE, 
#met_df2 <- data.frame()
met_df2 <- read.csv("met_data2_.csv")
stations3 <- read.csv("nearest_stations2_.csv")

start_row <- 1
end_row <- 358

system.time({
  # Iterate through the specified range of rows
  for (i in start_row:end_row) {
    
    station_id <- stations3$id[i]
    year <- stations3$Year[i]
    
    for (Year in 1840:2024) {
      
      if (year!=Year) {next} else {   
        
        met_data <- meteo_pull_monitors(unique(station_id),
                                        date_min = paste0(Year, "-03-01"),
                                        date_max = paste0(Year, "-11-30"),
                                        var = c("TMAX", "PRCP"))
        
        if (nrow(met_data) == 0 | !"tmax" %in% colnames(met_data) |  !"prcp" %in% colnames(met_data)) {
          id <- stations2$id[i]
          date <- NA
          prcp <- NA
          tmax <- NA
          plantid <- stations2$PlantID[i]
        }
        else {
          id <- met_data$id 
          date <- met_data$date
          prcp <- met_data$prcp
          tmax <- met_data$tmax
          plantid <- rep(stations2$PlantID[i],length(id))
        }
        
        
        # Create a new row for the results
        new_r <- data.frame(id = id,
                            date = date,
                            prcp = prcp,
                            tmax = tmax,
                            Year = Year,
                            plantid = plantid)
        
        # Append data to the batch data frame
        met_df2 <- rbind(met_df, new_r)
      }}
  }
})

# Save the batch results to an existing CSV file
output_file <- '/Users/sarah/OneDrive - Franklin & Marshall College/Documents/GitHub/Milkweed-phenology/met_data2_.csv'
write.csv(met_df2, file = output_file, row.names = FALSE)

######################################################
#To find a third closest station if the second has NA values
#create a subset
na2_subset <- met_df2 %>%
  filter(is.na(prcp) | is.na(tmax)) %>%
  dplyr::select(plantid) %>%
  distinct() %>%
  inner_join(HerbariumData_nona, by = join_by(plantid == Observation_ID))


# Initialize ONCE, 
#na_df2 <- data.frame()
na_df2 <- read.csv("nearest_stations3_.csv")

nrow(na2_subset)
# Define the range of rows, next start with 358
start_row <- 1
end_row <- 358

system.time({
  # Iterate through the specified range of rows
  for (i in start_row:end_row) {
    # Extract the plant_id, latitude, longitude, and year
    plant_id <- na2_subset$plantid[i]
    latitude <- na2_subset$Latitude[i]
    longitude <- na2_subset$Longitude[i]
    year <- na2_subset$Year[i]
    
    # Create a temporary data frame to follow mete_nearby_stations function format (necessary!)
    temp_df <- data.frame(id = plant_id, latitude = latitude, longitude = longitude)
    
    for (Year in 1840:2024) {
      
      if (year!=Year) {next} else {   
        # Call the function for the specific year
        stations <- meteo_nearby_stations(lat_lon_df = temp_df,
                                          lat_colname = "latitude",
                                          lon_colname = "longitude",
                                          var = "TMAX",
                                          year_min = year,
                                          year_max = year,
                                          limit = 3)
        
        # Extract the station name and distance from the first station in the list
        
        station_info <- stations[[1]][2,]
        station_name <- station_info$name[1]
        station_distance <- station_info$distance[1]
        station_id <- station_info$id[1]
        
        # Create a new row for the results
        new_row <- data.frame(id = station_id,
                              PlantID = plant_id,
                              Latitude = latitude,
                              Longitude = longitude,
                              Year = year,
                              StationName = station_name,
                              Distance = station_distance)
        
        # Append data to the batch data frame
        na_df2 <- rbind(na_df2, new_row)
      }}  
  }
})

# Save the batch results to an existing CSV file
output_file <- '/Users/sarah/OneDrive - Franklin & Marshall College/Documents/GitHub/Milkweed-phenology/nearest_stations3_.csv'
write.csv(na_df2, file = output_file, row.names = FALSE)

###find the met data for third closest station####

# Initialize ONCE, 
#met_df3 <- data.frame()
met_df3 <- read.csv("met_data3_.csv")
stations4 <- read.csv("nearest_stations3_.csv")

nrow(stations4)
start_row <- 1
end_row <- 358

system.time({
  # Iterate through the specified range of rows
  for (i in start_row:end_row) {
    
    station_id <- stations4$id[i]
    year <- stations4$Year[i]
    
    for (Year in 1840:2024) {
      
      if (year!=Year) {next} else {   
        
        met_data <- meteo_pull_monitors(unique(station_id),
                                        date_min = paste0(Year, "-03-01"),
                                        date_max = paste0(Year, "-11-30"),
                                        var = c("TMAX", "PRCP"))
        
        if (nrow(met_data) == 0 | !"tmax" %in% colnames(met_data) |  !"prcp" %in% colnames(met_data)) {
          id <- stations3$id[i]
          date <- NA
          prcp <- NA
          tmax <- NA
          plantid <- stations3$PlantID[i]
        }
        else {
          id <- met_data$id 
          date <- met_data$date
          prcp <- met_data$prcp
          tmax <- met_data$tmax
          plantid <- rep(stations2$PlantID[i],length(id))
        }
        
        
        # Create a new row for the results
        new_r <- data.frame(id = id,
                            date = date,
                            prcp = prcp,
                            tmax = tmax,
                            Year = Year,
                            plantid = plantid)
        
        # Append data to the batch data frame
        met_df3 <- rbind(met_df, new_r)
      }}
  }
})

# Save the batch results to an existing CSV file
output_file <- '/Users/sarah/OneDrive - Franklin & Marshall College/Documents/GitHub/Milkweed-phenology/met_data3_.csv'
write.csv(met_df3, file = output_file, row.names = FALSE)


##############################################
#### Dataset wrangling of NOAA_met_data ##############
#distance, station, plant id, tmax, prcp, date, year 

#dataset with stations and their met data
stations2 <- read.csv("nearest_stations_.csv")
met_df <- read.csv("met_data_.csv")
#minus the na data points, distance, long/lat
colnames(stations2)[colnames(stations2) == "PlantID"] <- "plantid"
data1 <- left_join(met_df, stations2 %>% dplyr::select(id, plantid, Distance), by = c("id", "plantid")) %>%
  filter(!is.na(prcp) | !is.na(tmax))

#dataset with second closest stations and their met data
stations3 <- read.csv("nearest_stations2_.csv")
met_df2 <- read.csv("met_data2_.csv")
colnames(stations3)[colnames(stations3) == "PlantID"] <- "plantid"
data2 <- left_join(met_df2, stations3 %>% dplyr::select(id, plantid, Distance), by = c("id", "plantid"))  %>%
  filter(!is.na(prcp) | !is.na(tmax))

#dataset with third closest stations and their met data
stations4 <- read.csv("nearest_stations3_.csv")
met_df3 <- read.csv("met_data3_.csv")
colnames(stations4)[colnames(stations4) == "PlantID"] <- "plantid"
data3 <- left_join(met_df3, stations4 %>% dplyr::select(id, plantid, Distance), by = c("id", "plantid")) 


#combine datasets data1 and data2
combined_df <- full_join(data1, data2, by = c("id", "plantid", "date", "Year"), suffix = c("_1", "_2"))

# Replace NA values in prcp, tmax, and Distance
combined_df <- combined_df %>%
  mutate(
    prcp = coalesce(prcp_1, prcp_2),
    tmax = coalesce(tmax_1, tmax_2),
    Distance = coalesce(Distance_1, Distance_2)
  ) %>%
  # Select the relevant columns
  dplyr::select(id, plantid, date, Year, prcp, tmax, Distance)


# Join data3 (third closest station) with the existing combined_df
combined_df <- full_join(combined_df, data3, by = c("id", "plantid", "date", "Year"), suffix = c("_2", "_3"))

# Replace NA values in prcp, tmax, and Distance using the third closest station (data3) for substitution
combined_df <- combined_df %>%
  mutate(
    prcp = coalesce(prcp_2, prcp_3),
    tmax = coalesce(tmax_2, tmax_3),
    Distance = coalesce(Distance_2, Distance_3)
  ) %>%
  # Select the relevant columns
  dplyr::select(id, plantid, date, Year, prcp, tmax, Distance)


#subset this to get only plants with a full growing season

#extract month
month_data <- combined_df %>%
  mutate(date = as.Date(date),    
         Month = month(date))  

# Filter only months from March (3) to November (11) - gs that we defined
growing_season <- month_data %>%
  filter(Month >= 3 & Month <= 11)

# Check for each plantid and year if all 9 months are present
valid_plants <- growing_season %>%
  group_by(plantid, Year) %>%
  summarise(months_present = n_distinct(Month), .groups = "drop") %>%
  filter(months_present == 9)

# Keep only rows for those valid plantid + Year combinations
gs_subset <- growing_season %>%
  semi_join(valid_plants, by = c("plantid", "Year"))


# Step 1: Get summary statistics
combined_df_summary <- gs_subset %>%
  group_by(plantid) %>%
  summarize(
    mean_prcp = mean(prcp, na.rm = TRUE),
    mean_tmax = mean(tmax, na.rm = TRUE),
    mean_distance = mean(Distance, na.rm = TRUE)
  ) %>%
  filter(!is.nan(mean_prcp) & !is.nan(mean_tmax) & !is.nan(mean_distance))

# Step 2: combined_df with station id, Join back to get the id where Distance equals mean_distance
combined_df_selected2 <- combined_df %>%
  inner_join(combined_df_summary, by = "plantid") %>%
  filter(Distance == mean_distance) %>%
  dplyr::select(plantid, id, mean_prcp, mean_tmax, mean_distance)

#step 3: get rid of multiples
combined_df_selected2 <- combined_df_selected2 %>%
  group_by(plantid) %>%
  slice(1) %>%
  ungroup()

#convert tmax from tenth of celcius to celcius 
combined_df_selected2 <- combined_df_selected2 %>%
  mutate(mean_tmax = mean_tmax / 10)

#check if any stations > 40 m away
combined_df_selected2 %>%
  filter(mean_distance > 40) %>%
  dplyr::select(id, mean_distance)


#####################################################
### Plots for visualization 


#uploading stations data
met_data <- read.csv("met_data_csv", header = TRUE)

names(plant_data_id)[1] <- "stations.id"
names(met_data)[1] <- "stations.id"

#calcuate means by station and year

# create year variable
met_data$Year <- substr(met_data$date, 1, 4)

stat_mean <- met_data %>%
  group_by(Year,stations.id) %>%
  summarize(mean_precip = mean(prcp, na.rm = TRUE),
            mean_tmax = mean(tmax, na.rm = TRUE))

status_intensity_observation_sub$Year <- substr(status_intensity_observation$Observation_Date, 1, 4)

names(status_intensity_observation_sub)[3] <- "plant_id"
names(stat_plant)[1] <- "stations.id"
stat_plant$plant_id <- as.integer(stat_plant$plant_id)

plant_status <- left_join(status_intensity_observation_sub,stat_plant,by = "plant_id")

plant_status_met <- left_join(plant_status,stat_mean, by = c("stations.id","Year"))



# Get the first observation date for each plant and year
first_observation <- plant_status_met %>%
  group_by(Year, plant_id, Phenophase_Status) %>%
  summarize(first_observation_date = min(Observation_Date)) %>%
  filter(Phenophase_Status==0)


library(lubridate)
library(ggplot2)

# convert date column to Date format
plant_status_met$Observation_Date <- as.Date(plant_status_met$Observation_Date)

# get the first observation date for each plant and year where phenophase_status is 1 followed by 0
first_observation <- plant_status_met %>%
  arrange(plant_id, Observation_Date) %>%
  group_by(Year, plant_id) %>%
  filter(Phenophase_Status == 1 & lead(Phenophase_Status) == 0) %>%
  summarize(first_observation_date = min(Observation_Date), Phenophase_Status = 1)

# create a new column for day of year
first_observation$day_of_year <- yday(first_observation$first_observation_date)

# filter for flowering phenophase
flowering_data <- plant_status_met %>%
  filter(Phenophase == "Flowering") %>%
  left_join(first_observation, by = c("plant_id", "Year", "Phenophase_Status"))

# calculate mean flowering time by year
mean_flowering <- flowering_data %>%
  group_by(Year) %>%
  summarize(mean_flowering_time = mean(yday(Observation_Date) - yday(first_observation_date)))

# plot mean flowering time by year
ggplot(mean_flowering, aes(x = Year, y = mean_flowering_time)) +
  geom_point() +
  labs(x = "Year", y = "Mean flowering time") +
  theme_bw()



#Plot for Mean temperature compared to mean flowering time
merged_temp_flowering <- merge(stat_mean, mean_flowering, by = "Year")


# plot mean flowering time compared to temperature
ggplot(merged_temp_flowering, aes(x = mean_tmax, y = mean_flowering_time)) +
  geom_point() +
  labs(x = "Average Temperature (Celsius)", y = "Mean flowering time") +
  theme_bw()


#Plot for Mean flowering date by precipitation
ggplot(merged_temp_flowering, aes(x = mean_precip, y = mean_flowering_time)) +
  geom_point() +
  labs(x = "Mean precipitation", y = "Mean flowering time") +
  theme_bw()


# Fit linear regression model for flowering date over time
flowering_model <- lm(julian_date ~ Year + Latitude + Longitude + mean_distance + Phenophase_detail, data = analyze_df)
summary(flowering_model) 

#linear model with elevation (not able to run yet)
flowering_model_elevation <- lm(julian_date ~ Year + Latitude + Longitude + mean_distance + Phenophase_detail + elevation_calc, data = analyze_df_elevation)
summary(flowering_model) 


ggplot(analyze_df, aes(x = Year, y = julian_date)) +
  geom_point(color = "blue", alpha = 0.6) +                    
  geom_smooth(method = "lm", color = "red") +    
  labs(title = "Flowering Date Over Time",       
       x = "Year",                               
       y = "Julian Date") +                      
  theme_minimal()   +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"))


# Fit linear regression model for the effect of climate (temp and precip) over time
climate_model <- lm(julian_date ~ mean_tmax + mean_prcp + Latitude + Longitude + mean_distance, data = analyze_df)
summary(climate_model)

#linear model for effects of climate (temp and precip) with elevation df
climate_model_elevation <- lm(julian_date ~ mean_tmax + mean_prcp + Latitude + Longitude + mean_distance + elevation_calc, data = analyze_df_elevation)
summary(climate_model)

# Scatter plot with regression line for tmax
ggplot(analyze_df, aes(y = mean_tmax, x = Year)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue") +
  labs(
    #title = "Effect of Maximum Temperature on Flowering Date",
    x = "Year",
    y = "Maximum Temperature (0.1°C)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"))

# Scatter plot with regression line for prcp
ggplot(analyze_df, aes(x = mean_prcp, y = Year)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue") +
  labs(
    #title = "Effect of Precipitation on Flowering Date",
    x = "Precipitation (0.1 mm)",
    y = "Year") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"))
