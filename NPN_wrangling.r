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


# #phen_data with only open flowers (this is not necessary, because we are correcting for this)
# phen_data_of <- phen_data %>%
#   filter(phen_data$Phenophase_Status== 1 & phen_data$Phenophase_Description == "Open flowers")
# 
# # get data for each plant and year where phenophase_status is 1 followed by 0
# phen_data_of_0_1 <- phen_data %>%
#   arrange(Individual_ID, Observation_Date) %>%
#   group_by(Year, Individual_ID) %>%
#   filter(Phenophase_Description == "Open flowers") %>%
#   filter(lead(Phenophase_Status == 0) & Phenophase_Status == 1)
# 
# phen_data_of_0_1$Phenophase_Description[grep("Initial growth",phen_data_of_0_1$Phenophase_Description)] <- "Initial Growth"
# phen_data_of_0_1$Phenophase_Description[grep("Open flowers",phen_data_of_0_1$Phenophase_Description)] <- "Open Flowers"
# 


#remove 2010 data (too few)
phen_data_of <- dplyr::filter(phen_data_of,Year>2010)
# phen_data_of_0_1 <- dplyr::filter(phen_data_of_0_1,Year>2010)


#only look at plants that show up in every month of the growing season 
#subset this to get only plants with a full growing season

#extract month
month_data_NPN <- phen_data_of %>%
  mutate(
    Observation_Date = ymd(Observation_Date),
    Month = month(Observation_Date)
  )

# Filter only months from March (3) to November (11) - gs that we defined
growing_season <- month_data_NPN %>%
  filter(Month >= 3 & Month <= 11)

# Check for each plantid and year if all 9 months are present
valid_plants_NPN <- growing_season %>%
  group_by(Observation_ID, Year) %>%
  summarise(months_present = n_distinct(Month), .groups = "drop") %>%
  filter(months_present == 9)

# Keep only rows for those valid plantid + Year combinations
phen_data_of <- phen_data_of %>%
  semi_join(valid_plants_NPN, by = c("Observation_ID", "Year"))

