test_plant <- data.frame(plant_data_id[1:3,])

stations_test <- meteo_nearby_stations(test_plant,
                                  "latitude",
                                  "longitude",
                                  var = "TMAX",
                                  year_min = 2014,
                                  year_max = 2022,
                                  limit = 1)
