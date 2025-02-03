#non corrected data: phen_data_0_1
#corrected data: bias_results_NPN
#combined by plant id and get rid of other data if extra

combined_data_final <- inner_join(bias_results_NPN, phen_data_0_1, by = "Observation_ID")

#Compare 
#bias_results_NPN
out_1 <- lmer(corrected ~ Latitude + Longitude + Elevation_s +
              Year_s + gs_temp_z + gs_precip_z +
              (1|Individual_ID)-1, data=combined_data_final)
summary(out_1)
Anova(out_1)


#phen_data_0_1
out_2 <- lmer(Day_of_Year ~ Latitude + Longitude + Elevation_s +
                Year_s + gs_temp_z + gs_precip_z +
                (1|Individual_ID)-1, data=combined_data_final)
summary(out_2)
Anova(out_2)


