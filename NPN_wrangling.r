require(rnoaa)
require(dplyr)
require(tidyverse)
require(lme4)
require(car)
require(emmeans)
require(lmerTest)

# data from US-NPN (https://data.usanpn.org/observations/get-started)
phen_data <- read.csv("status_intensity_observation_data.csv", header = TRUE)

#identify missing values
phen_data[phen_data==-9999] <- NA

# create year variable
phen_data$Year <- substr(phen_data$Observation_Date, 1, 4)

#calculate growing season averages (phen_data)
phen_data$gs_temp <- rowMeans(cbind(phen_data$Tmax_Spring,phen_data$Tmax_Summer,phen_data$Tmax_Fall))
phen_data$gs_precip <- rowMeans(cbind(phen_data$Prcp_Spring,phen_data$Prcp_Summer,phen_data$Prcp_Fall))

#standardize variables for analysis (phen_data)
phen_data$gs_temp_z <- scale(phen_data$gs_temp)
phen_data$gs_precip_z <- scale(phen_data$gs_precip)
phen_data$Year <- as.numeric(phen_data$Year)
phen_data$Year_s <- phen_data$Year-(min(phen_data_0_1$Year)+1)
phen_data$Elevation_s <- scale(phen_data$Elevation_in_Meters)


#phen_data with only open flowers
phen_data_of <- phen_data %>%
  filter(phen_data$Phenophase_Status== 1 & phen_data$Phenophase_Description == "Open flowers")

# subset only initial growth, open flowers, and ripe fruit
my_patterns <- c("Initial growth","Open flowers","Ripe fruits")
phen_data_ior <- dplyr::filter(phen_data,grepl(paste(my_patterns, collapse='|'),Phenophase_Description))

# get data for each plant and year where phenophase_status is 1 followed by 0
phen_data_of_0_1 <- phen_data_ior %>%
  arrange(Individual_ID, Observation_Date) %>%
  group_by(Year, Individual_ID) %>%
  filter(lead(Phenophase_Status == 0) & Phenophase_Status == 1)

phen_data_of_0_1$Phenophase_Description[grep("Initial growth",phen_data_of_0_1$Phenophase_Description)] <- "Initial Growth"
phen_data_of_0_1$Phenophase_Description[grep("Open flowers",phen_data_of_0_1$Phenophase_Description)] <- "Open Flowers"

#remove 2010 data (too few)
phen_data_of <- dplyr::filter(phen_data_of,Year>2010)
phen_data_of_0_1 <- dplyr::filter(phen_data_of_0_1,Year>2010)
