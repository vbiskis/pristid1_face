# Details ----
#' 07_new_pz_curve.R
#' Paper: [Fine-tuning established morphometric models...]
#' DOI: 10.1111/csp2.13308
#' Author: Nikki Biskis
#' Date: 2025-11-12
#' Output: New morphometric equation for P. zijsron
#' Dependencies: N/A
#' *Changes from original:
#' Pulled repetitive content from graphical parameters
#' -----------

library(boot)
all_morpho <- readRDS("morphodat.rds")
list2env(all_morpho, envir = .GlobalEnv)

#define function to calculate R-squared
rsq_function <- function(formula, data, indices) {
  d <- data[indices,] #allows boot to select sample
  fit <- lm(formula, data=d) #fit regression model
  return(summary(fit)$r.square) #return R-squared of model
}

#New curve and test - greenies

#Linear model----
gmodel <- lm(TL_curve ~ SRL, dfZ)
summary(gmodel)

par(mfrow = c(2,2)) #arranging plots in a 2x2 layout
plot(gmodel) #plotting the model
#fine as is

gmodel2 <- lm(log10(TL_curve) ~ log10(SRL), dfZ)
par(mfrow = c(2,2)) #arranging plots in a 2x2 layout
plot(gmodel2) #plotting the model
#worse

##PZ----
#perform bootstrapping with 2000 replications
repsR <- boot(data=dfZ, statistic=rsq_function, R=2000, formula=TL_curve~SRL)

#view results of boostrapping
repsR
plot(repsR)

boot.ci(repsR, type="bca")

#define function to calculate fitted regression coefficients
coef_function <- function(formula, data, indices) {
  d <- data[indices,] #allows boot to select sample
  fit <- lm(formula, data=d) #fit regression model
  return(coef(fit)) #return coefficient estimates of model
}

repsCo <- boot(data=dfZ, statistic=coef_function, R=2000, formula=TL_curve~SRL)

repsCo
plot(repsCo, index=1) #intercept of model
plot(repsCo, index=2) #disp predictor variable

#calculate adjusted bootstrap percentile (BCa) intervals
boot.ci(repsCo, type="bca", index=1) #intercept of model
boot.ci(repsCo, type="bca", index=2) #disp predictor variable

##AC----

#perform bootstrapping with 2000 replications
repsA <- boot(data=dfA, statistic=rsq_function, R=2000, formula=TL_curve~SRL)

#view results of boostrapping
repsA
plot(repsA) #eeeeee

boot.ci(repsA, type="bca")

repsCoA <- boot(data=dfA, statistic=coef_function, R=2000, formula=TL_curve~SRL)

repsCoA
plot(repsCoA, index=1) #intercept of model
plot(repsCoA, index=2) #disp predictor variable

#calculate adjusted bootstrap percentile (BCa) intervals
boot.ci(repsCoA, type="bca", index=1) #intercept of model
boot.ci(repsCoA, type="bca", index=2) #disp predictor variable

