require(rnoaa)
require(dplyr)

# data from US-NPN (https://data.usanpn.org/observations/get-started)
status_intensity_observation <- read.csv("status_intensity_observation_data - Copy.csv",header=T)

# create date variable (remove time and missing values)
status_intensity_observation$Date <- substr(status_intensity_observation$Update_Datetime,1,10)
status_intensity_observation <-status_intensity_observation[status_intensity_observation$Date!="-9999",]

#create year variable
status_intensity_observation$Year <- substr(status_intensity_observation$Date,1,4)

for (i in 1:length(unique(status_intensity_observation$Year))) {
  sub_data <- 
    subset(status_intensity_observation,status_intensity_observation$Year==unique(status_intensity_observation$Year)[i])
  assign(paste("plant_data",unique(status_intensity_observation$Year)[i],sep=""),sub_data)
  


# generate dataset of lat, long, and site ID (required format for meteo_nearby_stations function)
plant_data_id <- data.frame(plant_data$Site_ID,plant_data$Latitude,plant_data$Longitude)
names(plant_data_id) <- c("id","latitude","longitude")

plant_data_id<-plant_data_id[unique(match(unique(plant_data_id$id),plant_data_id$id)),]

# find nearby stations by latitude (and longitude?)
stations1 <- meteo_nearby_stations(plant_data_id,
                                   lat_colname = "latitude",
                                   lon_colname = "longitude",
                                   var = "TMAX",
                                   year_min=unique(status_intensity_observation$Year)[i],
                                   year_max=unique(status_intensity_observation$Year)[i],
                                   limit=1)


}


# generate dataset of lat, long, and site ID (required format for meteo_nearby_stations function)
plant_data_id_from2017 <- data.frame(status_intensity_observation$Site_ID,status_intensity_observation$Latitude,status_intensity_observation$Longitude)
names(plant_data_id_from2017) <- c("id","latitude","longitude")

plant_data_id_from2017<-plant_data_id_from2017[unique(match(unique(plant_data_id$id),plant_data_id$id)),]
  
  
# converts from list to tibble (data.frame)
stations <- do.call(rbind.data.frame,stations1)

for (i in 1:nrow(plant_data_id)) {
  data_sub <- subset(plant_data,plant_data$Site_ID==plant_data_id_from2017$id[i])

# pull meteorological data (march - november growing season) for stations by date
  
  for( i in 1: nrow(plant_data_id_from2017)){
  met_data_from2022 <- meteo_pull_monitors(stations$id[1],
                                date_min="2022-3-01",
                                date_max="2022-11-30",
                                var=c("TMAX","PRCP"))
  }
  for( i in 1: nrow(plant_data_id_from2017)){
    met_data_from2021 <- meteo_pull_monitors(stations$id[1],
                                             date_min="2021-3-01",
                                             date_max="2021-11-30",
                                             var=c("TMAX","PRCP"))
  }
  for( i in 1: nrow(plant_data_id_from2017)){
    met_data_from2020 <- meteo_pull_monitors(stations$id[1],
                                             date_min="2020-3-01",
                                             date_max="2020-11-30",
                                             var=c("TMAX","PRCP"))
  }for( i in 1: nrow(plant_data_id_from2017)){
    met_data_from2019 <- meteo_pull_monitors(stations$id[1],
                                             date_min="2019-3-01",
                                             date_max="2019-11-30",
                                             var=c("TMAX","PRCP"))
  }for( i in 1: nrow(plant_data_id_from2017)){
    met_data_from2018 <- meteo_pull_monitors(stations$id[1],
                                             date_min="2018-3-01",
                                             date_max="2018-11-30",
                                             var=c("TMAX","PRCP"))
  }for( i in 1: nrow(plant_data_id_from2017)){
    met_data_from2017 <- meteo_pull_monitors(stations$id[1],
                                             date_min="2017-3-01",
                                             date_max="2017-11-30",
                                             var=c("TMAX","PRCP"))
  }for( i in 1: nrow(plant_data_id_from2017)){
    met_data_from2016 <- meteo_pull_monitors(stations$id[1],
                                             date_min="2016-3-01",
                                             date_max="2016-11-30",
                                             var=c("TMAX","PRCP"))
  }
  
  
  # join stations data with met_data
  stat_met <- inner_join(met_data_from2022,stations,by="id")
  # join stations data with met_data
  stat_met <- inner_join(met_data_from2021,stations,by="id")
  # join stations data with met_data
  stat_met <- inner_join(met_data_from2020,stations,by="id")
  # join stations data with met_data
  stat_met <- inner_join(met_data_from2019,stations,by="id")
  # join stations data with met_data
  stat_met <- inner_join(met_data_from2018,stations,by="id")
  # join stations data with met_data
  stat_met <- inner_join(met_data_from2017,stations,by="id")
  # join stations data with met_data
  stat_met <- inner_join(met_data_from2016,stations,by="id")
  


# join met_data with plant_data
stations$Site_ID <- names(stations1) # first get site_ID into station data

# join stations data with met_data
stat_met <- inner_join(met_data,stations,by="id")

# join stat_met data with plant_data
stat_met$Date <- as.character(stat_met$date) # rename date to Date, so dataframes can merge
plant_data$Site_ID <- as.character(plant_data$Site_ID)
plant_data <- inner_join(plant_data,stat_met, by=c("Site_ID","Date"))




#Tasks for next Week:
#Create temperature and precipitation dataframe for the years 2017-2022
# Join dataframes together
