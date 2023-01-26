require(rnoaa)
require(dplyr)

# data from US-NPN (https://data.usanpn.org/observations/get-started)
plant_data <- read.csv("/Users/17163/Downloads/datasheet_1667486095636/status_intensity_observation_data.csv",header=T)

# create date variable (remove time and missing values)
plant_data$Date <- substr(plant_data$Update_Datetime,1,10)
plant_data <- plant_data[plant_data$Date!="-9999",]

# generate dataset of lat, long, and site ID (required format for meteo_nearby_stations function)
plant_data_id <- data.frame(plant_data$Site_ID,plant_data$Latitude,plant_data$Longitude)
names(plant_data_id) <- c("id","latitude","longitude")

plant_data_id<-plant_data_id[unique(match(unique(plant_data_id$id),plant_data_id$id)),]

# find nearby stations by latitude (and longitude?)
stations1 <- meteo_nearby_stations(plant_data_id,
                                 lat_colname = "latitude",
                                 lon_colname = "longitude",
                                 var = "TMAX",
                                 year_min=2022,
                                 year_max=2022,
                                 limit=1)

# converts from list to tibble (data.frame)
stations <- do.call(rbind.data.frame,stations1)

# generate date range data for each station
# range of dates for meteorological data from each station
datemin <-numeric(nrow(plant_data_id))
datemax <-numeric(nrow(plant_data_id))
for (i in 1:nrow(plant_data_id)) {
  data_sub <- subset(plant_data,plant_data$Site_ID==plant_data_id$id[i])
  datemin[i] <- data_sub$Date[1]
  datemax[i] <- data_sub$Date[nrow(data_sub)]
}

# pull meteorological data for stations by date
met_data <- meteo_pull_monitors(stations$id,
                    date_min=datemin,
                    date_max=datemax,
                    var="TMAX")


# join met_data with plant_data
stations$Site_ID <- names(stations1) # first get site_ID into station data

# join stations data with met_data
stat_met <- inner_join(met_data,stations,by="id")

# join stat_met data with plant_data
stat_met$Date <- as.character(stat_met$date) # rename date to Date, so dataframes can merge
plant_data$Site_ID <- as.character(plant_data$Site_ID)
plant_data <- inner_join(plant_data,stat_met, by=c("Site_ID","Date"))
