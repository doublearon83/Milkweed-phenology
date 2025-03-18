### Linear model of Herbarium_fl_use data (Herbarium data with met data from NOAA)

#packages
library(dplyr)

#data sets
bias_results_herbarium #herbarium_weibull
HerbariumData_fl_use #herbarium_weibull

#combine data
combined_data_herbarium <- inner_join(bias_results_herbarium, HerbariumData_fl_use, by = "Observation_ID")

################# Comparison of Original DOY and Corrected #######################

#1st percentile - corrected
combined_data_herbarium_0.01 <- combined_data_herbarium %>% filter(percentile == 0.01)

out_c_0.01_h <- lm(corrected_0.01 ~ Latitude + Longitude + Elevation_s +
                     Year_s , data=combined_data_herbarium_0.01)
summary(out_c_0.01_h)
Anova(out_c_0.01_h)
#1st percentile - original DOY
out_o_0.01_h <- lm(Day_of_Year ~ Latitude + Longitude + Elevation_s +
                     Year_s, data=combined_data_herbarium_0.01)
summary(out_o_0.01_h)
Anova(out_o_0.01_h)


#climate, no year, corrected
out_cc_0.01_h <- lm(corrected_0.01 ~ Latitude + Longitude + Elevation_s, data=combined_data_herbarium_0.01)
summary(out_cc_0.01_h)
Anova(out_cc_0.01_h)


#climate, no year, original DOY
out_oc_0.01_h <- lm(Day_of_Year ~ Latitude + Longitude + Elevation_s, data=combined_data_herbarium_0.01)
summary(out_oc_0.01_h)
Anova(out_oc_0.01_h)



#year, no climate, corrected
out_cy_0.01_h <- lm(corrected_0.01 ~ Latitude + Longitude + Elevation_s +
                      Year_s , data=combined_data_herbarium_0.01)
summary(out_cy_0.01_h)
Anova(out_cy_0.01_h)


#year, no climate, original DOY
out_oy_0.01_h <- lm(Day_of_Year ~ Latitude + Longitude + Elevation_s +
                      Year_s , data=combined_data_herbarium_0.01)
summary(out_oy_0.01_h)
Anova(out_oy_0.01_h)





#50th percentile - corrected
combined_data_herbarium_0.5 <- combined_data_herbarium %>% filter(percentile == 0.5)

out_c_0.5_h <- lm(corrected_0.5 ~ Latitude + Longitude + Elevation_s +
                    Year_s , data=combined_data_herbarium_0.5)
summary(out_c_0.5_h)
Anova(out_c_0.5_h)

#50th percentile - original DOY
out_o_0.5_h <- lm(Day_of_Year ~ Latitude + Longitude + Elevation_s +
                    Year_s , data=combined_data_herbarium_0.5)
summary(out_o_0.5_h)
Anova(out_o_0.5_h)


#climate, no year, corrected
out_cc_0.5_h <- lm(corrected_0.5 ~ Latitude + Longitude + Elevation_s, data=combined_data_herbarium_0.5)
summary(out_cc_0.5_h)
Anova(out_cc_0.5_h)

#climate, no year, original DOY
out_oc_0.5_h <- lm(Day_of_Year ~ Latitude + Longitude + Elevation_s , data=combined_data_herbarium_0.5)
summary(out_oc_0.5_h)
Anova(out_oc_0.5_h)



#year, no climate, corrected
out_cy_0.5_h <- lm(corrected_0.5 ~ Latitude + Longitude + Elevation_s +
                     Year_s , data=combined_data_herbarium_0.5)
summary(out_cy_0.5_h)
Anova(out_cy_0.5_h)

#year, no climate, original DOY
out_oy_0.5_h <- lm(Day_of_Year ~ Latitude + Longitude + Elevation_s +
                     Year_s , data=combined_data_herbarium_0.5)
summary(out_oy_0.5_h)
Anova(out_oy_0.5_h)




#99th percentile - corrected
combined_data_herbarium_0.99 <- combined_data_herbarium %>% filter(percentile == 0.99)

out_c_0.99_h <- lm(corrected_0.99 ~ Latitude + Longitude + Elevation_s +
                     Year_s, data=combined_data_herbarium_0.99)
summary(out_c_0.99_h)
Anova(out_c_0.99_h)

#99th percentile - original DOY

out_o_0.99_h <- lm(Day_of_Year ~ Latitude + Longitude + Elevation_s +
                     Year_s , data=combined_data_herbarium_0.99)
summary(out_o_0.99_h)
Anova(out_o_0.99_h)

#climate, no year, corrected
out_cc_0.99_h <- lm(corrected_0.99 ~ Latitude + Longitude + Elevation_s, data=combined_data_herbarium_0.99)
summary(out_cc_0.99_h)
Anova(out_cc_0.99_h)

#climate, no year, original DOY
out_oc_0.99_h <- lm(Day_of_Year ~ Latitude + Longitude + Elevation_s, data=combined_data_herbarium_0.99)
summary(out_oc_0.99_h)
Anova(out_oc_0.99_h)



#year, no climate, corrected
out_cy_0.99_h <- lm(corrected_0.99 ~ Latitude + Longitude + Elevation_s +
                      Year_s , data=combined_data_herbarium_0.99)
summary(out_cy_0.99_h)
Anova(out_cy_0.99_h)

#year, no climate, original DOY
out_oy_0.99_h <- lm(Day_of_Year ~ Latitude + Longitude + Elevation_s +
                      Year_s, data=combined_data_herbarium_0.99)
summary(out_oy_0.99_h)
Anova(out_oy_0.99_h)

