#pglmm in phyr
library(phyr)

#pglmm() in phyr
library(phyr)
library(ape)
phy.sp <- read.tree(path to tree)
pglmm_model <- pglmm(
  Day_of_Year ~ Latitude * Phenophase_Description + 
    Longitude * Phenophase_Description + 
    Elevation_s * Phenophase_Description + 
    Phenophase_Description * Year_s + 
    Phenophase_Description * gs_temp_z + 
    Phenophase_Description * gs_precip_z + 
    (1 | sp__) + # Random intercept for species. This captures the species-specific variability.
    (1 | site), #Random intercept for sites, if you have site information. If not, we can get rid of this
  data = phen_data_0_1,
  cov_ranef = list(sp = phy.sp),
  family = poisson
  bayes = TRUE #Uses Bayesian estimation
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
get_chelsea_data(categ = "clim", type = "bio", id = 1, path = "Users/sarah/Desktop/Milkweed/Data")



