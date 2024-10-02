#recreating the phenesse package Weibull estimator

require(phenesse)
require(fitdistrplus)

s_cybele <- subset(inat_examples, scientific_name == "Speyeria cybele")

weib_percentile(observations = s_cybele$doy, percentile = 0.5) 

s_cybele$doy

param2 <- fitdist(s_cybele$doy,"weibull")$estimate

est <- qweibull(0.5,param2[1],param2[2])

est2 <- numeric(500)
for (i in 1:500) {

rs <- qweibull(runif(length(s_cybele$doy)),param2[1],param2[2])

param3 <- fitdist(rs,"weibull")$estimate

est2[i] <- qweibull(0.5,param3[1],param3[2])

}

2*est-mean(est2)
