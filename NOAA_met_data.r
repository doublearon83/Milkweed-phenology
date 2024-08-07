####extracting meteorological data from NOAA

status_intensity_observation <- read.csv("status_intensity_observation_data.csv", header = TRUE)

#Creating plant_data_id
plant_id <- unique(status_intensity_observation$Individual_ID)
latitude <- status_intensity_observation$Latitude[match(plant_id,status_intensity_observation$Individual_ID)]
longitude <- status_intensity_observation$Longitude[match(plant_id,status_intensity_observation$Individual_ID)]
plant_data_id <- data.frame(plant_id,latitude,longitude)

names(plant_data_id)[1] <- "id"

#subsetting phenology dataet
status_intensity_observation_sub  <- status_intensity_observation[,c(4,5,13,15,16,18)]


######DO NOT RUN UNLESS UPDATING STATIONS#######
################################################
# code for finding nearby stations by latitude (and longitude?)
stations <- meteo_nearby_stations(plant_data_id,
                                  lat_colname = "latitude",
                                  lon_colname = "longitude",
                                  var = "TMAX",
                                  year_min = 2014,
                                  year_max = 2022,
                                  limit = 1)

# converts from list to tibble (data.frame)
stations2 <- do.call(rbind.data.frame, stations)
stations2$plant_id <- names(stations)
stat_plant <- stations2

#Saving Dataframe stations
write.csv(stat_plant, file = '/Users/kegem/Desktop/GitHub/Project13/Milkweed-phenology/stat_plant_csv', row.names = FALSE)

################################################################
# code for looping through each plant by year in Herbarium Data
#Herbarium Data
library(googlesheets4)
gs4_deauth()
HerbariumData <- read_sheet("https://docs.google.com/spreadsheets/d/13vqlJXSW35_sx9veANUHr0jn4caxJzPPwRhRyDKLGFo/edit#gid=0")

#check duplicates
HerbariumData[HerbariumData$Identification %in% HerbariumData$Identification[duplicated(HerbariumData$Identification)], ]


#packages
library(dplyr)
library(tidyr)
library(rnoaa)

#create a subset of HerbariumData that does not have na
#find the indices for the rows with missing values 
missing_rows <- which(is.na(HerbariumData$Year) | is.na(HerbariumData$Latitude) | is.na(HerbariumData$Longitude))
#make new dataframe with the filtered out rows with missing values
HerbariumData_nona <- HerbariumData[-missing_rows, ]

########### Batch Running Code to find nearest stations
# Initialize ONCE, 
#batch_results_df <- data.frame()
batch_results_df <- read.csv("nearest_stations.csv")

# Define the range of rows, next start with 639
start_row <- 1
end_row <- 636

system.time({
  # Iterate through the specified range of rows
  for (i in start_row:end_row) {
    # Extract the plant_id, latitude, longitude, and year
    plant_id <- HerbariumData_nona$Identification[i]
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
output_file <- '/Users/sarah/OneDrive - Franklin & Marshall College/Documents/GitHub/Milkweed-phenology/nearest_stations.csv'
write.csv(batch_results_df, file = output_file, row.names = FALSE)

#####################################################
#####################################################
#uploading stations data
stations <- read.csv("stations2", header = TRUE)

#uploading stat_plant_data
stat_plant <- read.csv("stat_plant", header = TRUE)


######DO NOT RUN UNLESS UPDATING Met data############
#####################################################
# pull meteorological data (march - november growing season) for stations by date

years <- seq(2016, 2022)
met_data <- lapply(years, function(years) {
  meteo_pull_monitors(unique(stations2$id),
                      date_min = paste0(years, "-03-01"),
                      date_max = paste0(years, "-11-30"),
                      var = c("TMAX", "PRCP"))
}) %>% bind_rows()

#Saving Dataframe met data

write.csv(met_data, file = '/Users/kegem/Desktop/GitHub/Project13/Milkweed-phenology/met_data_csv', row.names = FALSE)


############pull met using a loop ##################
# Initialize ONCE, 
#met_df <- data.frame()
met_df <- read.csv("met_data.csv")
stations2 <- read.csv("nearest_stations.csv")

#start with 639 next
start_row <- 1
end_row <- 636

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
output_file <- '/Users/sarah/OneDrive - Franklin & Marshall College/Documents/GitHub/Milkweed-phenology/met_data.csv'
write.csv(met_df, file = output_file, row.names = FALSE)

###########################################################
#To find a second station if the first has NA values
#create a subset
na_subset <- met_df %>%
  filter(is.na(prcp) | is.na(tmax)) %>%
  select(plantid) %>%
  distinct() %>%
  inner_join(HerbariumData_nona, by = join_by(plantid == Identification))

# Initialize ONCE, 
#na_df <- data.frame()
na_df <- read.csv("nearest_stations2.csv")

# Define the range of rows, next start with 
start_row <- 1
end_row <- 352

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
output_file <- '/Users/sarah/OneDrive - Franklin & Marshall College/Documents/GitHub/Milkweed-phenology/nearest_stations2.csv'
write.csv(na_df, file = output_file, row.names = FALSE)

###find the met data for second closest station####

# Initialize ONCE, 
#met_df2 <- data.frame()
met_df2 <- read.csv("met_data2.csv")
stations3 <- read.csv("nearest_stations2.csv")

start_row <- 1
end_row <- 636

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
output_file <- '/Users/sarah/OneDrive - Franklin & Marshall College/Documents/GitHub/Milkweed-phenology/met_data2.csv'
write.csv(met_df2, file = output_file, row.names = FALSE)


#### Dataset to use for analysis ##############
#distance, station, plant id, tmax, prcp, date, year 

#dataset with stations and their met data
stations2 <- read.csv("nearest_stations.csv")
met_df <- read.csv("met_data.csv")
#minus the na data points, distance, long/lat
colnames(stations2)[colnames(stations2) == "PlantID"] <- "plantid"
data1 <- left_join(met_df, stations2 %>% dplyr::select(id, plantid, Distance), by = c("id", "plantid")) %>%
  filter(!is.na(prcp) | !is.na(tmax))

#dataset with second closest stations and their met data
stations3 <- read.csv("nearest_stations2.csv")
met_df2 <- read.csv("met_data2.csv")
colnames(stations3)[colnames(stations3) == "PlantID"] <- "plantid"
data2 <- left_join(met_df2, stations3 %>% dplyr::select(id, plantid, Distance), by = c("id", "plantid"))

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


#for each plant id, find the mean prcp, max, and distance
combined_df_selected1 <- combined_df %>%
  group_by(plantid) %>%
  summarize(
    mean_prcp = mean(prcp, na.rm = TRUE),
    mean_tmax = mean(tmax, na.rm = TRUE),
    mean_distance = mean(Distance, na.rm = TRUE))%>%
  filter(!is.nan(mean_prcp) & !is.nan(mean_distance) & !is.nan(mean_tmax)) 
    
#join temp, prcp, distance data with HerbariumData_fl
combined_df_selected2 <- combined_df_selected1 %>%
  dplyr::select(plantid, mean_prcp, mean_tmax, mean_distance)

analyze_df <- left_join(HerbariumData_fl, combined_df_selected2, by = c("Identification" = "plantid"))%>%
  drop_na(Longitude, Latitude)

#addding elevation information
long_lat_df <- analyze_df[c(14,13)]%>%
  rename(x = Longitude , y = Latitude) 
library(rgbif)
analyze_df <- 
  mutate (
elevation_calculated <- elevation(
  input = NULL,
  latitude = long_lat_df$y,
  longitude = long_lat_df$x,
  elevation_model = "srtm3",
  username = Sys.getenv("sbetts")
))
 
library(elevatr)
get_elev_point(long_lat_df, src = "epqs")

# Calculate average growing season precip at tmax values
avg_precip <- analyze_df %>%
  group_by(mean_tmax) %>%
  summarize(avg_precip = mean(prcp, na.rm = TRUE))


# Fit linear regression model for flowering date over time
flowering_model <- lm(julian_date ~ Year + Latitude + Longitude + mean_distance, data = analyze_df)
summary(flowering_model) 

ggplot(analyze_df, aes(x = Year, y = julian_date)) +
  geom_point(color = "blue", size = 1) +                    
  geom_smooth(method = "lm", color = "red") +    
  labs(title = "Flowering Date Over Time",       
       x = "Year",                               
       y = "Julian Date") +                      
  theme_minimal()          


# Fit linear regression model for the effect of climate (temp and precip) over time
climate_model <- lm(julian_date ~ mean_tmax + mean_prcp + Latitude + Longitude + mean_distance, data = analyze_df)
summary(climate_model)

# Scatter plot with regression line for tmax
ggplot(analyze_df, aes(x = mean_tmax, y = julian_date)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Effect of Maximum Temperature on Flowering Date",
       x = "Maximum Temperature (tmax)",
       y = "Julian Date") +
  theme_minimal()

# Scatter plot with regression line for prcp
ggplot(analyze_df, aes(x = mean_prcp, y = julian_date)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Effect of Precipitation on Flowering Date",
       x = "Precipitation (prcp)",
       y = "Julian Date") +
  theme_minimal()



#####################################################
######################################################

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


################ code for vectorization (not being used) ###################################

# Step 1: Create dataset for the input
input_data <- 
  HerbariumData_nona %>%
  select(Identification, Latitude, Longitude, Year)

names(input_data)[1] <- "id"

# Step 2: Function to subset data without year
noyear_data <- function(og_data) {
  og_data %>%
    select(-Year)
}


# Step 3: Function to fetch meteo data for a specific year
meteo_funct <- function(og_data) {
  # temporary df
  subset_df <- noyear_data(og_data)
  
  Year <- og_data$Year
  
  # Call meteo_nearby_stations function 
  stations <- meteo_nearby_stations(
    lat_lon_df = subset_df,
    lat_colname = "Latitude",
    lon_colname = "Longitude",
    var = "TMAX", 
    year_min = Year,
    year_max = Year,
    limit = 1
  )
  
  station_info <- stations[[1]]
  station_name <- station_info$name[1]
  station_distance <- station_info$distance[1]
  
  c(
    PlantID = og_data$id,
    Latitude = og_data$Latitude,
    Longitude = og_data$Longitude,
    Year = og_data$Year,
    StationName = station_name,
    Distance = station_distance
  )
}

# Step 4: Apply meteo_funct using lapply
meteo_results <- apply(input_data[1,],1,meteo_funct)

final_results_df <- do.call(rbind, meteo_results)

###############################################################

