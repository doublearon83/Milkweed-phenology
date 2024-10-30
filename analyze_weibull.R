library(flexsurv)
library(survival)
library(ggplot2)
library(dplyr)
library(phenesse)

#merge data
phen_subset <- phen_data[, c("Observation_ID", "Latitude", "Longitude", "Elevation_in_Meters", "Tmax", "Day_of_Year")]
analysis_data <- left_join(bias_results, phen_subset, by = "Observation_ID")

#How shape and scale changes with parameters
############## Model 1: Weibull with Covariates ##########################
fl_time_covariates <- analysis_data$Original 

############## Model 2: Weibull without Covariates ##########################
surv_obj <- Surv(phen_data$Day_of_Year, phen_data$Phenophase_Status)
fit_without_covariates <- flexsurvreg(surv_obj ~ 1,  
                                      dist = "weibull", 
                                      data = phen_data)

shape_without_covariates <- exp(coef(fit_without_covariates)["shape"])
scale_without_covariates <- exp(coef(fit_without_covariates)["scale"])

analysis_data$fl_time_no_covariates <- qweibull(0.5, shape_without_covariates, scale_without_covariates)
analysis_data$fl_time_no_covariates <- qweibull(runif(nrow(phen_data)), shape_without_covariates, scale_without_covariates)

#Plotting

ggplot(analysis_data, aes(x = Tmax)) +
  geom_line(aes(y = fl_time_no_covariates, color = "Without Covariates"), size = 0.5) +
  geom_point(aes(y = fl_time_covariates, color = "With Covariates"), size = 0.5) +
  geom_smooth(method="lm",aes(x=analysis_data$Tmax,y=fl_time_covariates)) +
  labs(x = "Maximum Temperature (Tmax)", 
       y = "Estimated Flowering Time", 
       title = "Comparison of Flowering Times: With vs. Without Covariates") +
  scale_color_manual(values = c("Without Covariates" = "blue", "With Covariates" = "red")) +
  theme_minimal()

observations <- phen_data$Day_of_Year

estimate_flowering_time <- function(row) {
  return(phenesse::weib_percentile(observations, percentile = 0.5, iterations = 25))
}

analyze_df$phenesse_fl<- apply(phen_data, 1, estimate_flowering_time)


#compare to fl_time_covariates

ggplot(analysis_data, aes(x = Tmax)) +
  geom_line(aes(y = phenesse_fl, color = "Without Covariates"), size = 0.5) +
  geom_line(aes(y = fl_time_covariates, color = "With Covariates"), size = 0.5) +
  labs(x = "Maximum Temperature (Tmax)", 
       y = "Estimated Flowering Time", 
       title = "Comparison of Flowering Times: With vs. Without Covariates") +
  scale_color_manual(values = c("Phenesse Package" = "purple", "Our Package" = "red")) +
  theme_minimal()





