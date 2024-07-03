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

#packages
library(dplyr)
library(tidyr)
library(rnoaa)

#create a subset of HerbariumData that does not have na
#find the indices for the rows with missing values 
missing_rows <- which(is.na(HerbariumData$Year) | is.na(HerbariumData$Latitude) | is.na(HerbariumData$Longitude))
#make new dataframe with the filtered out rows with missing values
HerbariumData_nona <- HerbariumData[-missing_rows, ]

######## TO TEST LOOP ###############
HerbariumData_test <- HerbariumData_nona[4:6, ]

  
# Initialize an empty data frame
final_results_df <- data.frame()

system.time(
# Iterate through each plant(row)
for (i in 1:nrow(HerbariumData_test)) {
  # Extract the plant_id, latitude, longitude
  plant_id <- HerbariumData_nona$Identification[i]
  latitude <- HerbariumData_nona$Latitude[i]
  longitude <- HerbariumData_nona$Longitude[i]
  year <- HerbariumData_nona$Year[i]

  # Create a temporary data frame to follow mete_nearby_stations function format (necessary!)
  temp_df <- data.frame(id = plant_id, latitude = latitude, longitude = longitude)

# input_data <- HerbariumData_nona %>%
#     select(Identification, Latitude, Longitude, Year)%>%
#     rename(plant_id = Identification, latitude = Latitude, longitude = Longitude, year = Year)

  # Iterate through each year
  for (Year in 2001:2005) {
    
    if (year!=Year) {next} else {

    # Call the function
    stations <- meteo_nearby_stations(lat_lon_df = temp_df,
                                      lat_colname = "latitude",
                                      lon_colname = "longitude",
                                      var = "TMAX",
                                      year_min = Year,
                                      year_max = Year,
                                      limit = 1)  
    
    # Extract the station name from the first station in the list
    station_info <- stations[[1]]
    station_name <- station_info$name[1]
    station_distance <- station_info$distance[1]
    
    # Create a new row for the results
    new_row <- data.frame(PlantID = plant_id, 
                          Latitude = latitude, 
                          Longitude = longitude, 
                          Year = Year, 
                          StationName = station_name,
                          Distance = station_distance)
    
    # Append data
    final_results_df <- rbind(final_results_df, new_row)
    }
  }
})

################ code for vectorization ###################################

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

# Define the years interested in
years <- 2020:2022

system.time(
# Create the final results data frame by using lapply
final_results_df <- do.call(rbind, lapply(1:nrow(HerbariumData_test), function(i) {
  # Extract the plant_id, latitude, longitude
  plant_id <- HerbariumData_nona$Identification[i]
  latitude <- HerbariumData_nona$Latitude[i]
  longitude <- HerbariumData_nona$Longitude[i]
  
  # Collect results for each year
  year_results <- lapply(years, function(Year) {
    # Create a temporary data frame to follow meteo_nearby_stations function format
    temp_df <- data.frame(id = plant_id, latitude = latitude, longitude = longitude)
    
    # Call the function
    stations <- meteo_nearby_stations(lat_lon_df = temp_df,
                                      lat_colname = "latitude",
                                      lon_colname = "longitude",
                                      var = "TMAX",
                                      year_min = Year,
                                      year_max = Year,
                                      limit = 1)  
    
    # Extract values
    station_info <- stations[[1]]
    station_name <- station_info$name[1]
    station_distance <- station_info$distance[1]
    
    # Return a data frame for each year
    return(data.frame(PlantID = plant_id, 
                      Latitude = latitude, 
                      Longitude = longitude, 
                      Year = Year, 
                      StationName = station_name,
                      Distance = station_distance))
  })
  
  # Append data 
  do.call(rbind, year_results)
}))
)

lapply(herbariumData,meteo_funct,1)

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


######################################################
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
