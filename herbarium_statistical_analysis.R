### Linear model of Herbarium_fl data (Herbarium data with met data from NOAA)

#packages
library(tidyverse)

#data sets
bias_results_herbarium #herbarium_weibull
analyze_df #herbarium_weibull

#combine data
combined_data_herbarium <- inner_join(bias_results_herbarium, analyze_df, by = "Observation_ID")

################# Comparison of Original DOY and Corrected #######################

#1st percentile - corrected
combined_data_herbarium_0.01 <- combined_data_herbarium %>% filter(percentile == 0.01)

H_c_0.01 <- lm(corrected_0.01 ~ Latitude + Longitude + Elevation_s +
                     Year_s  + mean_tmax + mean_prcp, data=combined_data_herbarium_0.01)
summary(H_c_0.01)
Anova(H_c_0.01)
#1st percentile - original DOY
H_o_0.01 <- lm(Day_of_Year ~ Latitude + Longitude + Elevation_s +
                     Year_s  + mean_tmax + mean_prcp, data=combined_data_herbarium_0.01)
summary(H_o_0.01)
Anova(H_o_0.01)


#climate, no year, corrected
H_cc_0.01 <- lm(corrected_0.01 ~ Latitude + Longitude + Elevation_s + mean_tmax + mean_prcp, data=combined_data_herbarium_0.01)
summary(H_cc_0.01)
Anova(H_cc_0.01)


#climate, no year, original DOY
H_oc_0.01 <- lm(Day_of_Year ~ Latitude + Longitude + Elevation_s + mean_tmax + mean_prcp, data=combined_data_herbarium_0.01)
summary(H_oc_0.01)
Anova(H_oc_0.01)



#year, no climate, corrected
H_cy_0.01 <- lm(corrected_0.01 ~ Latitude + Longitude + Elevation_s +
                      Year_s , data=combined_data_herbarium_0.01)
summary(H_cy_0.01)
Anova(H_cy_0.01)


#year, no climate, original DOY
H_oy_0.01 <- lm(Day_of_Year ~ Latitude + Longitude + Elevation_s +
                      Year_s , data=combined_data_herbarium_0.01)
summary(H_oy_0.01)
Anova(H_oy_0.01)





#50th percentile - corrected
combined_data_herbarium_0.5 <- combined_data_herbarium %>% filter(percentile == 0.5)

H_c_0.5 <- lm(corrected_0.5 ~ Latitude + Longitude + Elevation_s +
                    Year_s  + mean_tmax + mean_prcp, data=combined_data_herbarium_0.5)
summary(H_c_0.5)
Anova(H_c_0.5)

#50th percentile - original DOY
H_o_0.5 <- lm(Day_of_Year ~ Latitude + Longitude + Elevation_s +
                    Year_s + mean_tmax + mean_prcp , data=combined_data_herbarium_0.5)
summary(H_o_0.5)
Anova(H_o_0.5)


#climate, no year, corrected
H_cc_0.5 <- lm(corrected_0.5 ~ Latitude + Longitude + Elevation_s + mean_tmax + mean_prcp, data=combined_data_herbarium_0.5)
summary(H_cc_0.5)
Anova(H_cc_0.5)

#climate, no year, original DOY
H_oc_0.5 <- lm(Day_of_Year ~ Latitude + Longitude + Elevation_s  + mean_tmax + mean_prcp, data=combined_data_herbarium_0.5)
summary(H_oc_0.5)
Anova(H_oc_0.5)



#year, no climate, corrected
H_cy_0.5 <- lm(corrected_0.5 ~ Latitude + Longitude + Elevation_s +
                     Year_s , data=combined_data_herbarium_0.5)
summary(H_cy_0.5)
Anova(H_cy_0.5)

#year, no climate, original DOY
H_oy_0.5 <- lm(Day_of_Year ~ Latitude + Longitude + Elevation_s +
                     Year_s , data=combined_data_herbarium_0.5)
summary(H_oy_0.5)
Anova(H_oy_0.5)




#99th percentile - corrected
combined_data_herbarium_0.99 <- combined_data_herbarium %>% filter(percentile == 0.99)

H_c_0.99 <- lm(corrected_0.99 ~ Latitude + Longitude + Elevation_s +
                     Year_s + mean_tmax + mean_prcp, data=combined_data_herbarium_0.99)
summary(H_c_0.99)
Anova(H_c_0.99)

#99th percentile - original DOY

H_o_0.99 <- lm(Day_of_Year ~ Latitude + Longitude + Elevation_s +
                     Year_s  + mean_tmax + mean_prcp, data=combined_data_herbarium_0.99)
summary(H_o_0.99)
Anova(H_o_0.99)

#climate, no year, corrected
H_cc_0.99 <- lm(corrected_0.99 ~ Latitude + Longitude + Elevation_s + mean_tmax + mean_prcp, data=combined_data_herbarium_0.99)
summary(H_cc_0.99)
Anova(H_cc_0.99)

#climate, no year, original DOY
H_oc_0.99 <- lm(Day_of_Year ~ Latitude + Longitude + Elevation_s + mean_tmax + mean_prcp, data=combined_data_herbarium_0.99)
summary(H_oc_0.99)
Anova(H_oc_0.99)



#year, no climate, corrected
H_cy_0.99 <- lm(corrected_0.99 ~ Latitude + Longitude + Elevation_s +
                      Year_s , data=combined_data_herbarium_0.99)
summary(H_cy_0.99)
Anova(H_cy_0.99)

#year, no climate, original DOY
H_oy_0.99 <- lm(Day_of_Year ~ Latitude + Longitude + Elevation_s +
                      Year_s, data=combined_data_herbarium_0.99)
summary(H_oy_0.99)
Anova(H_oy_0.99)

