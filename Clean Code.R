#Newest Updates.

require(rnoaa)
require(dplyr)
require(tidyverse)

# data from US-NPN (https://data.usanpn.org/observations/get-started)
status_intensity_observation <- read.csv("status_intensity_observation_data - Copy.csv", header = TRUE)

# create date variable (remove time and missing values)
status_intensity_observation$Date <- substr(status_intensity_observation$Update_Datetime, 1, 10)
status_intensity_observation <- status_intensity_observation[status_intensity_observation$Date != "-9999",]

# create year variable
status_intensity_observation$Year <- substr(status_intensity_observation$Date, 1, 4)

#Creating plant_data_id
plant_id <- unique(status_intensity_observation$Individual_ID)
latitude <- status_intensity_observation$Latitude[match(plant_id,status_intensity_observation$Individual_ID)]
longitude <- status_intensity_observation$Longitude[match(plant_id,status_intensity_observation$Individual_ID)]
plant_data_id <- data.frame(plant_id,latitude,longitude)

names(plant_data_id)[1] <- "id"

#subsettig phenology dataet
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
write.csv(stations, file = '/Users/kegem/Desktop/GitHub/Project13/Milkweed-phenology/stations_csv', row.names = FALSE)

#####################################################
#####################################################

#uploading stations data
stations <- read.csv("stations_csv", header = TRUE)


######DO NOT RUN UNLESS UPDATING Met data############
#####################################################
# pull meteorological data (march - november growing season) for stations by date
years <- seq(2016, 2022)
met_data <- lapply(years, function(years) {
  meteo_pull_monitors(unique(stations$id),
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


# join met_data with plant_data
plant_meteo_data <- merge(plant_data_id, met_data, by = "stations.id")

stat_plant <- data.frame(plant_data_id,stations$id)



names(stat_plant)[1] <- "id"


# merge the stat_plant data with met_data by id
plant_meteo_data <- merge(stat_plant, met_data[,c("stations.id","date","tmax","prcp")], by = "stations.id")



# join stat_mean data with status_intesnity_observation_sub
plant_meteo_stations_data <- inner_join(plant_meteo_data,status_intensity_observation_sub, by="id")

#calcuate means by station and year

# create year variable
met_data$Year <- substr(met_data$date, 1, 4)

tapply(met_data$tmax,met_data[,c("id","Year")],mean,na.rm=T)

stat_mean <- met_data %>%
  
  group_by(Year,id) %>%
  summarize(mean_precip = mean(prcp, na.rm = TRUE),
            mean_tmax = mean(tmax, na.rm = TRUE))

#change Individual_ID to id
names(status_intensity_observation_sub)[3] <- "id"
status_intensity_observation_sub$id <- as.integer(status_intensity_observation_sub$id)
stat_mean$id <- as.integer(stat_mean$id)

# join stat_mean data with status_intesnity_observation_sub
plant_meteo_data <- inner_join(stat_mean,status_intensity_observation_sub,by="stations.id")

merged_data <- merge(status_intensity_observation_sub, plant_meteo_data[c("id", "tmax", "prcp")], by = "id")

