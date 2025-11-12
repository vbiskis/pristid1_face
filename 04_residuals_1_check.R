# Details ----
#' 04_residuals_1_check.R
#' Paper: [Fine-tuning established morphometric models...]
#' DOI: 10.1111/csp2.13308
#' Author: Nikki Biskis
#' Date: 2025-11-12
#' Purpose: Check normality all species
#' *Changes from original:
#'  used function to simplify tests
#'  could def automate more, but enjoy looking at them separately
#' -----------

source('src_stats_helper.R')

# Normality----
## AC----
check_norm(dfAR, cols = c("TLCalc_SRL", #nah
                          "Residuals_SRL", #nope
                          "NetRes_S", #right skew
                          "logResS", #mmm left
                          "StResS", #alright
                          "StNetResS")) #right skew

dfAR$Publication <- as.factor(dfAR$Publication)
leveneTest(TLCalc_SRL ~ Publication, data = dfAR) #equal var TL
leveneTest(Residuals_SRL ~ Publication, data = dfAR) #equal var raw res
leveneTest(NetRes_S ~ Publication, data = dfAR) #equal var net res
leveneTest(logResS ~ Publication, data = dfAR) #unequal log res!
leveneTest(StResS ~ Publication, data = dfAR) #equal var percent
leveneTest(StNetResS ~ Publication, data = dfAR) #unequal var net percent!

## PC----
check_norm(dfCR, cols = c("TLCalc_SRL", #nah
                          "Residuals_SRL", #nope
                          "NetRes_S", #right skew
                          "logResS", #better
                          "StResS", #right skew
                          "StNetResS")) #right again

dfCR$Publication <- as.factor(dfCR$Publication)
leveneTest(TLCalc_SRL ~ Publication, data = dfCR) #equal var TL
leveneTest(Residuals_SRL ~ Publication, data = dfCR) #equal var raw res
leveneTest(NetRes_S ~ Publication, data = dfCR) #unequal var net res
leveneTest(logResS ~ Publication, data = dfCR) #equal log res
leveneTest(StResS ~ Publication, data = dfCR) #equal var percent
leveneTest(StNetResS ~ Publication, data = dfCR) #unequal var net percent

## PP----
check_norm(dfPR, cols = c("TLCalc_SRL", #no no no
                          "Residuals_SRL", #nope
                          "NetRes_S", #oh no haha
                          "logResS", #pretty good!
                          "StResS", #almost
                          "StNetResS")) #yikes

dfPR$Publication <- as.factor(dfPR$Publication)
leveneTest(TLCalc_SRL ~ Publication, data = dfPR) #equal var TL
leveneTest(Residuals_SRL ~ Publication, data = dfPR) #equal var raw res
leveneTest(NetRes_S ~ Publication, data = dfPR) #equal var net res
leveneTest(logResS ~ Publication, data = dfPR) #equal log res
leveneTest(StResS ~ Publication, data = dfPR) #unequal var percent
leveneTest(StNetResS ~ Publication, data = dfPR) #equal var net percent

##so PP and PC logres are normal----
## PZ----
check_norm(dfZR, cols = c("TLCalc_SRL", #no no no
                          "Residuals_SRL", #nope
                          "NetRes_S", #oh no haha
                          "logResS", #pretty good!
                          "StResS", #that looks great!
                          "StNetResS")) #not quite

dfZR$Publication <- as.factor(dfZR$Publication)
leveneTest(TLCalc_SRL ~ Publication, data = dfZR) #equal var TL
leveneTest(Residuals_SRL ~ Publication, data = dfZR) #unequal var raw res
leveneTest(NetRes_S ~ Publication, data = dfZR) #unequal var net res
leveneTest(logResS ~ Publication, data = dfZR) #unequal log res
leveneTest(StResS ~ Publication, data = dfZR) #equal var percent
leveneTest(StNetResS ~ Publication, data = dfZR) #equal var net percent

#running non-parametric, all except PP which n >> than other species
