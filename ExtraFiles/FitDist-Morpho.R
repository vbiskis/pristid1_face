#Fit Dist
###LogTrans----
ggqqplot(log(dfR$Residuals_SRL))
ggdensity(log(dfR$Residuals_SRL),
          main = "Density plot of Residuals",
          xlab = "Residuals")

ggqqplot(log(dfR$NetResid_SRL))
ggdensity(log(dfR$NetResid_SRL),
          main = "Density plot of Residuals",
          xlab = "Residuals")

ggqqplot(dfR$LogSRL)
ggdensity(dfR$LogSRL,
          main = "Density plot of LogResiduals",
          xlab = "LogResiduals")
shapiro.test(dfR$LogSRL) #notnormal

install.packages("fitdistrplus")
install.packages("logspline")
library(fitdistrplus)
library(logspline)

ggdensity(dfR$Residuals_SRL,
          main = "Density plot of Residuals",
          xlab = "Residuals")

descdist(as.numeric(na.omit(dfR$Residuals_SRL)), boot = 1000, discrete = FALSE)
fit.norm <- fitdist(as.numeric(na.omit(dfR$Residuals_SRL)), "norm")
plot(fit.norm)

install.packages("gamlss")
library(gamlss)
fit <- fitDist(as.numeric(na.omit(dfR$Residuals_SRL)), k = 2, type = "realplus", trace = FALSE, try.gamlss = TRUE)

