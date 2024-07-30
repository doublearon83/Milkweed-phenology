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
#mean annual air temperature in celcius (0.1)


# To load and process GIS data
require(sp)
require(rgdal)
require(raster)
require(ncdf4)
require(sf)
#To make nicer looking maps
require(maps) 
require(mapdata)
require(RColorBrewer)

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

# Convert to sf object 
lat_long_sf <- st_as_sf(lat_long_, coords = c("longitude", "latitude"), crs = crs(tif_file))

# Extract values
extracted_values_sf <- extract(tif_file, lat_long_sf)
lat_long_sf$Temperature <- extracted_values_sf

print(lat_long_sf)


### The sp package
tif_file <- raster("C:/Users/sarah/Desktop/CHELSA_bio10_01.tif")
#spatial points
lonlat <- HerbariumData_nona[, c(13,14)]
pts <- SpatialPoints(lonlat)
class (pts)
showDefault(pts) #shows whats inside of pts
#no CRS so can provide one to the object 
crdref <- CRS('+proj=longlat +datum=WGS84')
pts <- SpatialPoints(lonlat, proj4string=crdref)
#We can use the SpatialPoints object to create a SpatialPointsDataFrame object
tempvalue <- runif(nrow(lonlat), min=0, max=100)
df <- data.frame(ID=1:nrow(lonlat), temp=tempvalue)
df
#Combine the SpatialPoints with the data.frame
ptsdf <- SpatialPointsDataFrame(pts, data=df)
ptsdf
#show whats inside
showDefault(ptsdf) 

### The sf package, need a shapefile

## can us raster and sp to download WorldClim data
library(raster)
library(sp)

r <- getData("worldclim",var="bio",res=10)
#Bio 1 and Bio12 are mean anual temperature and anual precipitation:
  r <- r[[c(1,12)]]
names(r) <- c("Temp","Prec")

#can create a SpatialPoint object using coordinates
points <- spsample(as(r@extent, 'SpatialPolygons'),n=100, type="random")    

values <- extract(r,points) #create a df

df <- cbind.data.frame(coordinates(points),values)




