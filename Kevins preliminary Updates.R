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

# create empty data frame to store combined data frames
combined_data <- data.frame()
  
  
  plant_id <- unique(status_intensity_observation$Individual_ID)
  latitude <- status_intensity_observation$Latitude[match(plant_id,status_intensity_observation$Individual_ID)]
  longitude <- status_intensity_observation$Longitude[match(plant_id,status_intensity_observation$Individual_ID)]
  plant_data_id <- data.frame(plant_id,latitude,longitude)
  
  names(plant_data_id)[1] <- "id"
  
  
  # find nearby stations by latitude (and longitude?)
  stations <- meteo_nearby_stations(plant_data_id,
                                    lat_colname = "latitude",
                                    lon_colname = "longitude",
                                    var = "TMAX",
                                    year_min = 2014,
                                    year_max = 2022,
                                    limit = 1)
  
  # converts from list to tibble (data.frame)
  stations <- do.call(rbind.data.frame, stations)
  
  # pull meteorological data (march - november growing season) for stations by date
  years <- seq(2016, 2022)
  met_data <- lapply(years, function(years) {
    meteo_pull_monitors(unique(stations$id),
                        date_min = paste0(years, "-03-01"),
                        date_max = paste0(years, "-11-30"),
                        var = c("TMAX", "PRCP"))
  }) %>% bind_rows()
  
  #Dataframe containing max preecipitation and temperature values for
  met_data_combo <- rbind(met_data_from2016, met_data_from2017, met_data_from2018, met_data_from2019, met_data_from2020, met_data_from2021, met_data_from2022)
  
  
  # join met_data with plant_data
  stat_met <- inner_join(met_data_combo, plant_data_id, by = "id")
  
  # combine data frames for each year
  combined_data <- rbind(combined_data, stat_met)
  }
  
  # create a data frame of all unique station IDs
  station_ids <- unique(stations$id)
  
  # create an empty data frame to store the meteorological data
  met_data <- data.frame()
  
  # loop through each station and pull the meteorological data for the years 2017-2022
  for (i in 1:length(station_ids)) {
    temp_data <- meteo_pull_monitors(station_ids[i],
                                     date_min="2017-03-01",
                                     date_max="2022-11-30",
                                     var=c("TMAX","PRCP"))
    temp_data$station_id <- station_ids[i] # add a column for the station ID
    met_data <- rbind(met_data, temp_data) # append the data to the met_data data frame
  }
  
  # remove duplicates and missing values
  met_data <- distinct(met_data, station_id, date, .keep_all = TRUE)
  met_data <- filter(met_data, !is.na(TMAX) & !is.na(PRCP))
  
  
  # calculate mean precipitation and Tmax per station for the growing season
  growing_season <- filter(station_data, date_min = "2017-03-01" & date_max = "2022-11-30")
  growing_season_summary <- group_by(growing_season, station_id) %>%
    summarize(mean_precip = mean(PRCP), mean_tmax = mean(TMAX))
  
  # join the plant data with the growing season summary
  plant_data_with_summary <- left_join(plant_data, growing_season_summary, by = "station_id")
  

  # calculate the mean temperature and precipitation for each plant
  plant_data_with_summary_summary <- group_by(plant_data_with_summary, Site_ID) %>%
    summarize(mean_precip = mean(mean_precip), mean_tmax = mean(mean_tmax))
  
#save dataframes  using write.csv(dataframe, file = '/Users/admin/newfile_csv' row.names = FALSE)