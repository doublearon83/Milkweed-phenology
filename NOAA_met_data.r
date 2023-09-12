####extracting meteorological data from NOAA

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
