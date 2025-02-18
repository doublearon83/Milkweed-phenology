#use NPN_analysis to load prior data set

#use met data to compare contemporary to historical values

phen_data_0_1$Individual_ID_z <- scale(phen_data_0_1$Individual_ID)
phen_data_0_1$Individual_ID <- as.factor(phen_data_0_1$Individual_ID)
out <- lmer(Day_of_Year ~ Latitude*Phenophase_Description + Longitude*Phenophase_Description + Elevation_s*Phenophase_Description +
              Phenophase_Description/Year_s + Phenophase_Description/gs_temp_z + Phenophase_Description/gs_precip_z +
              (1|Individual_ID) -1, data=phen_data_0_1)
summary(out)
Anova(out)


out <- lmer(Day_of_Year ~ Latitude*Phenophase_Description + Longitude*Phenophase_Description + Elevation_s*Phenophase_Description +
              Phenophase_Description*Year_s + Phenophase_Description*gs_temp_z + Phenophase_Description*gs_precip_z +
              (1|Individual_ID), data=phen_data_0_1)
summary(out)
Anova(out)

###### weibull distribution #############
weibull_model_phen <- vglm(Day_of_Year ~ Latitude * Phenophase_Description + Longitude * Phenophase_Description + 
                             Elevation_s * Phenophase_Description + Phenophase_Description * Year_s + 
                             Phenophase_Description * gs_temp_z + Phenophase_Description * gs_precip_z,
                           family = weibullR, data = phen_data_0_1)
summary(weibull_model_phen)
Anova(weibull_model_phen)





#load ggplot2 before making plot
require(ggplot2)

(LG_plot <- ggplot(phen_data_0_1, aes(x = gs_temp_z, y = Day_of_Year)) +
    geom_point(size = 3, color = "blue") +
    geom_line(aes(x = phen_data_0_1$gs_temp_z, y = predict(out)), size = 1.25, alpha = 0.5) +
    theme_bw() +
    facet_wrap(~ Year, scales = "free") +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 12, face = "plain"),                        
          panel.grid = element_blank(),                                                 
          plot.margin = unit(c(1,1,1,1), units = , "cm"))
)

###################################################################################
############ Possible Code for GGplot Without Missing Values #####################
#find the indices for the rows with missing values 
missing_rows <- which(is.na(phen_data_0_1$gs_temp_z) | is.na(phen_data_0_1$Day_of_Year))
#make new dataframe with the filtered out rows with missing values
filtered_df <- phen_data_0_1[-missing_rows, ]

filtered_df$pred <- 295+-12.0356*filtered_df$gs_temp_z+-0.9255*filtered_df$Year_s


#run the ggplot
(LG_plot_ <- ggplot(filtered_df, aes(x = gs_temp_z, y = Day_of_Year)) +
    geom_point(size = 3, color = "blue") +
    geom_line(aes(y = pred), size = 1.25, alpha = 0.5) +
    theme_bw() +
    facet_wrap(~ Year, scales = "free") +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 12, face = "plain"),                        
          panel.grid = element_blank(),                                                 
          plot.margin = unit(c(1,1,1,1), units = "cm")))
################################################################################


#just open flowers
phen_data_0_1_fl <- dplyr::filter(phen_data_0_1,Phenophase_Description=="Open Flowers")

phen_data_0_1_fl <- phen_data_0_1_fl %>%
  arrange(Individual_ID, Observation_Date) %>%
  group_by(Year, Individual_ID) %>%
  filter(lead(Phenophase_Status == 0) & Phenophase_Status == 1)

out <- lmer(Day_of_Year ~ Year_s*gs_temp_z + Year_s*gs_precip_z + Latitude + Longitude + Elevation_in_Meters + (1|Individual_ID) -1, data=phen_data_0_1_fl)
summary(out)


#0 before 1 for JUST open flowers

phen_data_ior$Phenophase_Description[grep("Open flowers",phen_data_ior$Phenophase_Description)] <- "Open Flowers"

# get data for each plant and year where phenophase_status is 1 followed by 0
phen_data_0_1_fl <- phen_data_ior %>%
  arrange(Individual_ID, Observation_Date) %>%
  group_by(Year, Individual_ID) %>%
  filter(Phenophase_Description== "Open Flowers" & lead(Phenophase_Status == 0) & Phenophase_Status == 1)

#calculate growing season averages
phen_data_0_1_fl$gs_temp <- rowMeans(cbind(phen_data_0_1_fl$Tmax_Spring,phen_data_0_1_fl$Tmax_Summer,phen_data_0_1_fl$Tmax_Fall))
phen_data_0_1_fl$gs_precip <- rowMeans(cbind(phen_data_0_1_fl$Prcp_Spring,phen_data_0_1_fl$Prcp_Summer,phen_data_0_1_fl$Prcp_Fall))

#standardize variables for analysis
phen_data_0_1_fl$gs_temp_z <- scale(phen_data_0_1_fl$gs_temp)
phen_data_0_1_fl$gs_precip_z <- scale(phen_data_0_1_fl$gs_precip)
phen_data_0_1_fl$Year <- as.numeric(phen_data_0_1_fl$Year)
phen_data_0_1_fl$Year_s <- phen_data_0_1_fl$Year-(min(phen_data_0_1_fl$Year)+1)
phen_data_0_1_fl$Elevation_s <- scale(phen_data_0_1_fl$Elevation_in_Meters)


#remove 2010 data (too few)
phen_data_0_1_fl <- dplyr::filter(phen_data_0_1_fl,Year>2010)

out <- lmer(Day_of_Year ~ Year_s*gs_temp_z + Year_s*gs_precip_z + Latitude + Longitude + Elevation_in_Meters + (1|Individual_ID) -1, data=phen_data_0_1_fl)
summary(out)


