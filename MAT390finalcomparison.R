#non corrected data: phen_data_0_1
#corrected data: bias_results_NPN
#combined by plant id and get rid of other data if extra
library(VGAM)

combined_data_final <- inner_join(bias_results_NPN, phen_data_0_1, by = "Observation_ID")


#Compare these using 

weibull_model_notcorrected <- vglm(Day_of_Year ~ Latitude * Phenophase_Description + Longitude * Phenophase_Description + 
                             Elevation_s * Phenophase_Description + Phenophase_Description * Year_s + 
                             Phenophase_Description * gs_temp_z + Phenophase_Description * gs_precip_z,
                           family = weibullR, data = combined_data_final)
summary(weibull_model_notcorrected)
Anova(weibull_model_notcorrected)

weibull_model_phen_corrected <- vglm(corrected ~ Latitude * Phenophase_Description + Longitude * Phenophase_Description + 
                             Elevation_s * Phenophase_Description + Phenophase_Description * Year_s + 
                             Phenophase_Description * gs_temp_z + Phenophase_Description * gs_precip_z,
                           family = weibullR, data = combined_data_final)
summary(weibull_model_phen_corrected)
Anova(weibull_model_phen_corrected)


