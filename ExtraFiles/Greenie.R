#New curve and test - greenies

#Get Ready----

#Load dataset
library(readxl)
PZ <- read_xlsx("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/USC/Papers/2. Morpho Paper/Morpho-Master.xlsx", sheet = 'PZ')
PP <- read_xlsx("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/USC/Papers/2. Morpho Paper/Morpho-Master.xlsx", sheet = 'PP')

#Linear model
gmodel <- lm(TL_curve ~ SRL, PZ)
summary(gmodel)

par(mfrow = c(2,2)) #arranging plots in a 2x2 layout
plot(gmodel) #plotting the model
#fine as is

gmodel2 <- lm(log10(TL_curve) ~ log10(SRL), PZ)
par(mfrow = c(2,2)) #arranging plots in a 2x2 layout
plot(gmodel2) #plotting the model
#messes up worse

##Method 1----

#Sampling the data
sample.data <-
 PZ[sample(nrow(PZ), 10, replace = TRUE), ]
sample.model <- lm(TL_curve ~ SRL, data = sample.data)
summary(sample.model)

#The bootstrap regression
sample_coef_intercept <- NULL
sample_coef_x1 <- NULL

for (i in 1:10000) {
  sample_d = sample.data[sample(1:nrow(sample.data), nrow(sample.data), replace = TRUE), ]
  
  model_bootstrap <- lm(TL_curve ~ SRL, data = sample_d)
  
  sample_coef_intercept <-
    c(sample_coef_intercept, model_bootstrap$coefficients[1])
  
  sample_coef_x1 <-
    c(sample_coef_x1, model_bootstrap$coefficients[2])
}

coefs <- rbind(sample_coef_intercept, sample_coef_x1)

# Combining the results in a table
means.boot = c(mean(sample_coef_intercept), mean(sample_coef_x1))
knitr::kable(round(
  cbind(
    population = coef(summary(gmodel))[, 1],
    sample = coef(summary(sample.model))[, 1],
    bootstrap = means.boot),4), 
  "simple", caption = "Coefficients in different models")

confint(gmodel)
confint(sample.model)

a <-
  cbind(
    quantile(sample_coef_intercept, prob = 0.025),
    quantile(sample_coef_intercept, prob = 0.975))
b <-
  cbind(quantile(sample_coef_x1, prob = 0.025),
        quantile(sample_coef_x1, prob = 0.975))
c <-
  round(cbind(
    population = confint(gmodel),
    sample = confint(sample.model),
    boot = rbind(a, b)), 4)
colnames(c) <- c("2.5 %", "97.5 %",
                 "2.5 %", "97.5 %",
                 "2.5 %", "97.5 %")
knitr::kable(rbind(
  c('population',
    'population',
    'sample',
    'sample',
    'bootstrap',
    'bootstrap'),c))

##Method 2----

library(boot)

#define function to calculate R-squared
rsq_function <- function(formula, data, indices) {
  d <- data[indices,] #allows boot to select sample
  fit <- lm(formula, data=d) #fit regression model
  return(summary(fit)$r.square) #return R-squared of model
}
#perform bootstrapping with 2000 replications
repsR <- boot(data=PZ, statistic=rsq_function, R=2000, formula=TL_curve~SRL)

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

repsCo <- boot(data=PZ, statistic=coef_function, R=2000, formula=TL_curve~SRL)

repsCo
plot(repsCo, index=1) #intercept of model
plot(repsCo, index=2) #disp predictor variable

#calculate adjusted bootstrap percentile (BCa) intervals
boot.ci(repsCo, type="bca", index=1) #intercept of model
boot.ci(repsCo, type="bca", index=2) #disp predictor variable

##PP

#perform bootstrapping with 2000 replications
repsP <- boot(data=PP, statistic=rsq_function, R=2000, formula=TL_curve~SRL)

#view results of boostrapping
repsP
plot(repsP)

boot.ci(repsP, type="bca")

repsCoP <- boot(data=PP, statistic=coef_function, R=2000, formula=TL_curve~SRL)

repsCoP
plot(repsCo, index=1) #intercept of model
plot(repsCo, index=2) #disp predictor variable

#calculate adjusted bootstrap percentile (BCa) intervals
boot.ci(repsCoP, type="bca", index=1) #intercept of model
boot.ci(repsCoP, type="bca", index=2) #disp predictor variable

##AC

#perform bootstrapping with 2000 replications
repsA <- boot(data=AC, statistic=rsq_function, R=2000, formula=TL_curve~SRL)

#view results of boostrapping
repsA
plot(repsA)

boot.ci(repsA, type="bca")

repsCoA <- boot(data=AC, statistic=coef_function, R=2000, formula=TL_curve~SRL)

repsCoA
plot(repsCoA, index=1) #intercept of model
plot(repsCoA, index=2) #disp predictor variable

#calculate adjusted bootstrap percentile (BCa) intervals
boot.ci(repsCoA, type="bca", index=1) #intercept of model
boot.ci(repsCoA, type="bca", index=2) #disp predictor variable

