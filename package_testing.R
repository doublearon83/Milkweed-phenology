#elastic net
library(elasticnet)
library(lars)

#pglmm() in phyr -- NOT USING
library(phyr)
pglmm_model <- pglmm(
  Day_of_Year ~ Latitude * Phenophase_Description + 
    Longitude * Phenophase_Description + 
    Elevation_s * Phenophase_Description + 
    Phenophase_Description * Year_s + 
    Phenophase_Description * gs_temp_z + 
    Phenophase_Description * gs_precip_z + 
    (1 | site),   # Random intercept for sites,
  data = phen_data_0_1,
  family = "poisson" #generalized linear mixed models
)

# Predict the Day_of_Year
phen_data_0_1$predicted_day_of_year <- predict(pglmm_model, type = "response")


#VGAM
library(VGAM)
library(ggplot2)
#using our data
weibull_model <- vglm(Day_of_Year ~ Latitude * Phenophase_Description + Longitude * Phenophase_Description + 
                        Elevation_s * Phenophase_Description + Phenophase_Description * Year_s + 
                        Phenophase_Description * gs_temp_z + Phenophase_Description * gs_precip_z,
                      family = weibullR, data = phen_data_0_1)
phen_data_0_1$predicted_day_of_year <- predict(weibull_model, type = "response")

####################################
ggplot(phen_data_0_1, aes(x = Day_of_Year, y = predicted_day_of_year)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Observed vs. Predicted Day of Year",
       x = "Observed Day of Year",
       y = "Predicted Day of Year") +
  theme_minimal()
##################################

#CHELSEA
install.packages("glue")
library(glue)
install.packages("remotes")
remotes::install_github("inSileco/rchelsa")
library(rchelsa)
ls("package:rchelsa")

#Download data
get_chelsea_data(categ = "clim", type = "bio", id = 1, path = ".") #downloads a tif file
#mean annual air temperature in celcius

#load/ read in a tif file
library(raster)
library(sp)
library(sf)
tif_file <- raster("C:/Users/sarah/Desktop/CHELSA_bio10_01.tif")
print(tif_file)
plot(tif_file)
attributes(tif_file)

lat_long_ <- HerbariumData_nona[, c(13,14)]
coordinates(lat_long_) <- ~Longitude + Latitude

# Extract raster values
extracted_values <- extract(tif_file, lat_long_)
lat_long_$Temperature <- extracted_values

print(lat_long_)

# Convert to sf object (for more modern spatial handling)
lat_long_sf <- st_as_sf(lat_long_, coords = c("longitude", "latitude"), crs = crs(tif_file))

# Extract values
extracted_values_sf <- extract(tif_file, lat_long_sf)
lat_long_sf$Temperature <- extracted_values_sf

print(lat_long_sf)












