library(lme4)

#non corrected data: phen_data and phen_data_0_1
#corrected data: bias_results_NPN (combined data set of 0.01, 0.5, 0.99 from weibull_NPN)

combined_data_phenof <- inner_join(bias_results_NPN, phen_data_of, by = "Observation_ID")
combined_data_phenof_01 <- inner_join(bias_results_NPN, phen_data_of_0_1, by = "Observation_ID")

#only look at dates 2014-2022
combined_data_phenof <- combined_data_phenof %>%
  filter(Year >=2014 & Year <= 2022)

combined_data_phenof_01 <-combined_data_phenof_01 %>%
  filter(Year >= 2014 & Year <= 2022)



################# phen_data_of : Comparison of Original DOY and Corrected #######################

#1st percentile - corrected
combined_data_phenof_0.01 <- combined_data_phenof %>% filter(percentile == 0.01)

NPN_phen_c_0.01 <- lmer(corrected_0.01 ~ Latitude + Longitude + Elevation_s +
                          Year_s + gs_temp_z + gs_precip_z +
                          (1|Individual_ID), data=combined_data_phenof_0.01)
summary(NPN_phen_c_0.01)
Anova(NPN_phen_c_0.01)
#1st percentile - original DOY
NPN_phen_o_0.01 <- lmer(Day_of_Year ~ Latitude + Longitude + Elevation_s +
                          Year_s + gs_temp_z + gs_precip_z +
                          (1|Individual_ID), data=combined_data_phenof_01)
summary(NPN_phen_o_0.01)
Anova(NPN_phen_o_0.01)


#climate, no year, corrected
NPN_phen_cc_0.01 <- lmer(corrected_0.01 ~ Latitude + Longitude + Elevation_s +
                           gs_temp_z + gs_precip_z + 
                           (1|Individual_ID), data=combined_data_phenof_0.01)
summary(NPN_phen_cc_0.01)
Anova(NPN_phen_cc_0.01)


#climate, no year, original DOY
NPN_phen_oc_0.01 <- lmer(Day_of_Year ~ Latitude + Longitude + Elevation_s +
                           gs_temp_z + gs_precip_z + 
                           (1|Individual_ID), data=combined_data_phenof_0.01)
summary(NPN_phen_oc_0.01)
Anova(NPN_phen_oc_0.01)



#year, no climate, corrected
NPN_phen_cy_0.01 <- lmer(corrected_0.01 ~ Latitude + Longitude + Elevation_s +
                           Year_s + 
                           (1|Individual_ID), data=combined_data_phenof_0.01)
summary(NPN_phen_cy_0.01)
Anova(NPN_phen_cy_0.01)


#year, no climate, original DOY
NPN_phen_oy_0.01 <- lmer(Day_of_Year ~ Latitude + Longitude + Elevation_s +
                           Year_s + 
                           (1|Individual_ID), data=combined_data_phenof_0.01)
summary(NPN_phen_oy_0.01)
Anova(NPN_phen_oy_0.01)


#50th percentile - corrected
combined_data_phenof_0.5 <- combined_data_phenof %>% filter(percentile == 0.5)

NPN_phen_c_0.5 <- lmer(corrected_0.5 ~ Latitude + Longitude + Elevation_s +
                         Year_s + gs_temp_z + gs_precip_z +
                         (1|Individual_ID), data=combined_data_phenof_0.5)
summary(NPN_phen_c_0.5)
Anova(NPN_phen_c_0.5)

#50th percentile - original DOY
NPN_phen_o_0.5 <- lmer(Day_of_Year ~ Latitude + Longitude + Elevation_s +
                         Year_s + gs_temp_z + gs_precip_z +
                         (1|Individual_ID), data=combined_data_phenof_0.5)
summary(NPN_phen_o_0.5)
Anova(NPN_phen_o_0.5)


#climate, no year, corrected
NPN_phen_cc_0.5 <- lmer(corrected_0.5 ~ Latitude + Longitude + Elevation_s +
                          gs_temp_z + gs_precip_z + 
                          (1|Individual_ID), data=combined_data_phenof_0.5)
summary(NPN_phen_cc_0.5)
Anova(NPN_phen_cc_0.5)

#climate, no year, original DOY
NPN_phen_oc_0.5 <- lmer(Day_of_Year ~ Latitude + Longitude + Elevation_s +
                          gs_temp_z + gs_precip_z + 
                          (1|Individual_ID), data=combined_data_phenof_0.5)
summary(NPN_phen_oc_0.5)
Anova(NPN_phen_oc_0.5)



#year, no climate, corrected
NPN_phen_cy_0.5 <- lmer(corrected_0.5 ~ Latitude + Longitude + Elevation_s +
                          Year_s + 
                          (1|Individual_ID), data=combined_data_phenof_0.5)
summary(NPN_phen_cy_0.5)
Anova(NPN_phen_cy_0.5)

#year, no climate, original DOY
NPN_phen_oy_0.5 <- lmer(Day_of_Year ~ Latitude + Longitude + Elevation_s +
                          Year_s + 
                          (1|Individual_ID), data=combined_data_phenof_0.5)
summary(NPN_phen_oy_0.5)
Anova(NPN_phen_oy_0.5)








#99th percentile - corrected
combined_data_phenof_0.99 <- combined_data_phenof %>% filter(percentile == 0.99)

NPN_phen_c_0.99 <- lmer(corrected_0.99 ~ Latitude + Longitude + Elevation_s +
                          Year_s + gs_temp_z + gs_precip_z +
                          (1|Individual_ID), data=combined_data_phenof_0.99)
summary(NPN_phen_c_0.99)
Anova(NPN_phen_c_0.99)

#99th percentile - original DOY (phen_data)

NPN_phen_o_0.99 <- lmer(Day_of_Year ~ Latitude + Longitude + Elevation_s +
                          Year_s + gs_temp_z + gs_precip_z +
                          (1|Individual_ID), data=combined_data_phenof_0.99)
summary(NPN_phen_o_0.99)
Anova(NPN_phen_o_0.99)

#climate, no year, corrected
NPN_phen_cc_0.99 <- lmer(corrected_0.99 ~ Latitude + Longitude + Elevation_s +
                           gs_temp_z + gs_precip_z + 
                           (1|Individual_ID), data=combined_data_phenof_0.99)
summary(NPN_phen_cc_0.99)
Anova(NPN_phen_cc_0.99)

#climate, no year, original DOY
NPN_phen_oc_0.99 <- lmer(Day_of_Year ~ Latitude + Longitude + Elevation_s +
                           gs_temp_z + gs_precip_z + 
                           (1|Individual_ID), data=combined_data_phenof_0.99)
summary(NPN_phen_oc_0.99)
Anova(NPN_phen_oc_0.99)



#year, no climate, corrected
NPN_phen_cy_0.99 <- lmer(corrected_0.99 ~ Latitude + Longitude + Elevation_s +
                           Year_s + 
                           (1|Individual_ID), data=combined_data_phenof_0.99)
summary(NPN_phen_cy_0.99)
Anova(NPN_phen_cy_0.99)

#year, no climate, original DOY
NPN_phen_oy_0.99 <- lmer(Day_of_Year ~ Latitude + Longitude + Elevation_s +
                           Year_s + 
                           (1|Individual_ID), data=combined_data_phenof_0.99)
summary(NPN_phen_oy_0.99)
Anova(NPN_phen_oy_0.99)



################# phen_data_of_0_1 : Comparison of Original DOY and Corrected #######################
combined_phenof01_0.01 <- combined_data_phenof_01 %>% filter(percentile == 0.01)


#1st percentile - corrected
NPN_phen_01_c_0.01 <- lmer(corrected_0.01 ~ Latitude + Longitude + Elevation_s +
                             Year_s + gs_temp_z + gs_precip_z +
                             (1|Individual_ID), data=combined_phenof01_0.01)
summary(NPN_phen_01_c_0.01)
Anova(NPN_phen_01_c_0.01)
#1st  percentile - original DOY
NPN_phen_01_o_0.01 <- lmer(Day_of_Year ~ Latitude + Longitude + Elevation_s +
                             Year_s + gs_temp_z + gs_precip_z +
                             (1|Individual_ID), data=combined_phenof01_0.01)
summary(NPN_phen_01_o_0.01)
Anova(NPN_phen_01_o_0.01)


#climate, no year, corrected
NPN_phen_01_cc_0.01 <- lmer(corrected_0.01 ~ Latitude + Longitude + Elevation_s +
                              gs_temp_z + gs_precip_z + 
                              (1|Individual_ID), data=combined_phenof01_0.01)
summary(NPN_phen_01_cc_0.01)
Anova(NPN_phen_01_cc_0.01)


#climate, no year, original DOY
NPN_phen_01_oc_0.01 <- lmer(Day_of_Year ~ Latitude + Longitude + Elevation_s +
                              gs_temp_z + gs_precip_z + 
                              (1|Individual_ID), data=combined_phenof01_0.01)
summary(NPN_phen_01_oc_0.01)
Anova(NPN_phen_01_oc_0.01)



#year, no climate, corrected
NPN_phen_01_cy_0.01 <- lmer(corrected_0.01 ~ Latitude + Longitude + Elevation_s +
                              Year_s + 
                              (1|Individual_ID), data=combined_phenof01_0.01)
summary(NPN_phen_01_cy_0.01)
Anova(NPN_phen_01_cy_0.01)


#year, no climate, original DOY
NPN_phen_01_oy_0.01 <- lmer(Day_of_Year ~ Latitude + Longitude + Elevation_s +
                              Year_s + 
                              (1|Individual_ID), data=combined_phenof01_0.01)
summary(NPN_phen_01_oy_0.01)
Anova(NPN_phen_01_oy_0.01)





#50th percentile - corrected
combined_phenof01_0.5 <- combined_data_phenof_01 %>% filter(percentile == 0.5)

NPN_phen_01_c_0.5 <- lmer(corrected_0.5 ~ Latitude + Longitude + Elevation_s +
                            Year_s + gs_temp_z + gs_precip_z +
                            (1|Individual_ID), data=combined_phenof01_0.5)
summary(NPN_phen_01_c_0.5)
Anova(NPN_phen_01_c_0.5)

#50th percentile - original DOY
NPN_phen_01_o_0.5 <- lmer(Day_of_Year ~ Latitude + Longitude + Elevation_s +
                            Year_s + gs_temp_z + gs_precip_z +
                            (1|Individual_ID), data=combined_phenof01_0.5)
summary(NPN_phen_01_o_0.5)
Anova(NPN_phen_01_o_0.5)


#climate, no year, corrected
NPN_phen_01_cc_0.5 <- lmer(corrected_0.5 ~ Latitude + Longitude + Elevation_s +
                             gs_temp_z + gs_precip_z + 
                             (1|Individual_ID), data=combined_phenof01_0.5)
summary(NPN_phen_01_cc_0.5)
Anova(NPN_phen_01_cc_0.5)

#climate, no year, original DOY
NPN_phen_01_oc_0.5 <- lmer(Day_of_Year ~ Latitude + Longitude + Elevation_s +
                             gs_temp_z + gs_precip_z + 
                             (1|Individual_ID), data=combined_phenof01_0.5)
summary(NPN_phen_01_oc_0.5)
Anova(NPN_phen_01_oc_0.5)



#year, no climate, corrected
NPN_phen_01_cy_0.5 <- lmer(corrected_0.5 ~ Latitude + Longitude + Elevation_s +
                             Year_s + 
                             (1|Individual_ID), data=combined_phenof01_0.5)
summary(NPN_phen_01_cy_0.5)
Anova(NPN_phen_01_cy_0.5)

#year, no climate, original DOY
NPN_phen_01_oy_0.5 <- lmer(Day_of_Year ~ Latitude + Longitude + Elevation_s +
                             Year_s + 
                             (1|Individual_ID), data=combined_phenof01_0.5)
summary(NPN_phen_01_oy_0.5)
Anova(NPN_phen_01_oy_0.5)








#99th percentile - corrected
combined_phenof01_0.99 <- combined_data_phenof_01 %>% filter(percentile == 0.99)

NPN_phen_01_c_0.99 <- lmer(corrected_0.99 ~ Latitude + Longitude + Elevation_s +
                             Year_s + gs_temp_z + gs_precip_z +
                             (1|Individual_ID), data=combined_phenof01_0.99)
summary(NPN_phen_01_c_0.99)
Anova(NPN_phen_01_c_0.99)

#99th percentile - original DOY (phen_data)

NPN_phen_01_o_0.99 <- lmer(Day_of_Year ~ Latitude + Longitude + Elevation_s +
                             Year_s + gs_temp_z + gs_precip_z +
                             (1|Individual_ID), data=combined_phenof01_0.99)
summary(NPN_phen_01_o_0.99)
Anova(NPN_phen_01_o_0.99)

#climate, no year, corrected
NPN_phen_01_cc_0.99 <- lmer(corrected_0.99 ~ Latitude + Longitude + Elevation_s +
                              gs_temp_z + gs_precip_z + 
                              (1|Individual_ID), data=combined_phenof01_0.99)
summary(NPN_phen_01_cc_0.99)
Anova(NPN_phen_01_cc_0.99)

#climate, no year, original DOY
NPN_phen_01_oc_0.99 <- lmer(Day_of_Year ~ Latitude + Longitude + Elevation_s +
                              gs_temp_z + gs_precip_z + 
                              (1|Individual_ID), data=combined_phenof01_0.99)
summary(NPN_phen_01_oc_0.99)
Anova(NPN_phen_01_oc_0.99)



#year, no climate, corrected
NPN_phen_01_cy_0.99 <- lmer(corrected_0.99 ~ Latitude + Longitude + Elevation_s +
                              Year_s + 
                              (1|Individual_ID), data=combined_phenof01_0.99)
summary(NPN_phen_01_cy_0.99)
Anova(NPN_phen_01_cy_0.99)

#year, no climate, original DOY
NPN_phen_01_oy_0.99 <- lmer(Day_of_Year ~ Latitude + Longitude + Elevation_s +
                              Year_s + 
                              (1|Individual_ID), data=combined_phenof01_0.99)
summary(NPN_phen_01_oy_0.99)
Anova(NPN_phen_01_oy_0.99)







####################################################################################


