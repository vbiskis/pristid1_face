#Begin----
##Install----

if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(gridGraphics)){install.packages("gridGraphics")}
if(!require(lsr)){install.packages("lsr")}
if(!require(lme4)){install.packages("lme4")}
if(!require(parameters)){install.packages("parameters")}
if(!require(see)){install.packages("see")}
if(!require(vtable)){install.packages("vtable")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(gplots)){install.packages("gplots")}
if(!require(grid)){install.packages("grid")}
if(!require(pwr)){install.packages("pwr")}
if(!require(ggpubr)){install.packages("ggpubr")}

##Load Data----
library(readxl)

ImgJ <-read_xlsx("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/USC/Papers/2. Morpho Paper/Morpho-Master.xlsx", sheet = 'Image J Comp')
I1 <-read_xlsx("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/USC/Papers/2. Morpho Paper/Morpho-Master.xlsx", sheet = 'IJ1')
I2 <-read_xlsx("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/USC/Papers/2. Morpho Paper/Morpho-Master.xlsx", sheet = 'IJ2')

AC <- read_xlsx("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/USC/Papers/2. Morpho Paper/Morpho-Master.xlsx", sheet = 'AC')
PC <- read_xlsx("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/USC/Papers/2. Morpho Paper/Morpho-Master.xlsx", sheet = 'PC')
PP <- read_xlsx("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/USC/Papers/2. Morpho Paper/Morpho-Master.xlsx", sheet = 'PP')
PZ <- read_xlsx("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/USC/Papers/2. Morpho Paper/Morpho-Master.xlsx", sheet = 'PZ')
AllS <- read_xlsx("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/USC/Papers/2. Morpho Paper/Morpho-Master.xlsx", sheet = 'AllSpec')
AllR <- read_xlsx("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/USC/Papers/2. Morpho Paper/Morpho-Master.xlsx", sheet = 'AllRes')

Atest <- read_xlsx("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/USC/Papers/2. Morpho Paper/Morpho-Master.xlsx", sheet = 'AnoxyTest - ALL')
Ares <- read_xlsx("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/USC/Papers/2. Morpho Paper/Morpho-Master.xlsx", sheet = 'Anoxy Resid')
Ztest <- read_xlsx("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/USC/Papers/2. Morpho Paper/Morpho-Master.xlsx", sheet = 'ZijsronTest - ALL')
Zres <- read_xlsx("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/USC/Papers/2. Morpho Paper/Morpho-Master.xlsx", sheet = 'Zijsron Resid')
Ptest <- read_xlsx("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/USC/Papers/2. Morpho Paper/Morpho-Master.xlsx", sheet = 'PristisTest - ALL')
Pres <- read_xlsx("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/USC/Papers/2. Morpho Paper/Morpho-Master.xlsx", sheet = 'Pristis Resid')
Ctest <- read_xlsx("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/USC/Papers/2. Morpho Paper/Morpho-Master.xlsx", sheet = 'ClavataTest - ALL')
Cres <- read_xlsx("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/USC/Papers/2. Morpho Paper/Morpho-Master.xlsx", sheet = 'Clavata Resid')

##AsDataFrame----
dfI <- data.frame(ImgJ)
dfI1 <- data.frame(I1)
dfI2 <- data.frame(I2)

df1 <- data.frame(AllS)
df2 <- data.frame(AllR)
dfA <- data.frame(AC)
dfC <- data.frame(PC)
dfP <- data.frame(PP)
dfZ <- data.frame(PZ)

dfAT <- data.frame(Atest)
dfZT <- data.frame(Ztest)
dfPT <- data.frame(Ptest)
dfCT <- data.frame(Ctest)
dfAR <- data.frame(Ares)
dfZR <- data.frame(Zres)
dfPR <- data.frame(Pres)
dfCR <- data.frame(Cres)

##Functions----
###Anoxy----
####Define----
AFariaL <- function(SRL){
  TL <- SRL*4.8904-75.012
  return(TL)
}
AFariaP <- function(SRL){
  TL <- 2.5626*SRL^1.1052
  return(TL)
}
APev <- function(SRL) {
  TL <- (5.2961*SRL)-72.683
  return(TL)
}
AWhittyL <- function(SRL){
  TL <- SRL/0.2
  return(TL)
}
AWhittyQ <- function(SRL){
  TL <- (0.1795+sqrt(0.00036*SRL-0.071208))/0.00018
  length(TL) == length(SRL)
  return(TL)
}
ABiskisL <- function(SRL){
  TL <- 4.0167*SRL + 167.41
  return(TL)
}
ABiskisP <- function(SRL){
  TL <- 8.3644*(SRL^0.8953)
  return(TL)
}
ABiskisQ <- function(SRL){
  TL <- -0.002*SRL^2+5.64*SRL-84.5
  return(TL)
}
BootA <- function(SRL){
  TL <- 4.0564*SRL + 188.07
  return(TL)
}

###Clavata----

CFariaL <- function(SRL){
  TL <- SRL*5.8895-170.44
  return(TL)
}
CFariaP <- function(SRL){
  TL <- 1.6671*SRL^1.2085
  return(TL)
}
CPev <- function(SRL) {
  TL <- (8.16*SRL)-437.5
  return(TL)
}
CThor <- function(SRL){
  TL <- 1.8173*(SRL^1.1799)
  return(TL)
}
CWhittyL <- function(SRL){
  TL <- SRL/0.19
  return(TL)
}
CWhittyP <- function(SRL){
  TL <- 2.213*(SRL^1.157)
  return(TL)
}
CBiskisL <- function(SRL){
  TL <- 6.1443*SRL - 166.83
  return(TL)
}
CBiskisP <- function(SRL){
  TL <- 2.7587*(SRL^1.1215)
  return(TL)
}

###Pristis----

PFariaL <- function(SRL){
  TL <- SRL*5.460-289.37
  return(TL)
}
PFariaP <- function(SRL){
  TL <- 0.9913*SRL^1.2526
  return(TL)
}
PMorg <- function(SRL){
  TL <- 5.2579*SRL - 180.708
  return(TL)
}
PPev <- function(SRL) {
  TL <- (4.1598*SRL)+121.49
  return(TL)
}
PThorF <- function(SRL){
  TL <- 2.5304*(SRL^1.0973)
  return(TL)
}
PThorM <- function(SRL){
  TL <- 1.281*(SRL^1.2107)
  return(TL)
}
PWhittyL <- function(SRL){
  TL <- SRL/0.23
  return(TL)
}
PWhittyP <- function(SRL){
  TL <- 1.943*(SRL^1.143)
  return(TL)
}
PBiskisL <- function(SRL){
  TL <- 4.6131*SRL - 87.584
  return(TL)
}
PBiskisP <- function(SRL){
  TL <- 3.2309*(SRL^1.0495)
  return(TL)
}

###Zijsron----

ZFariaL <- function(SRL){
  TL <- SRL*3.7666+166.15
  return(TL)
}
ZFariaP <- function(SRL){
  TL <- 5.0242*SRL^0.9704
  return(TL)
}
ZLearF <- function(SRL){
  TL <- 2.71828^(1.02*log(SRL)+1.29)
  return(TL)
}
ZLearM <- function(SRL){
  TL <- 2.71828^(1.05*log(SRL)+1.08)
  return(TL)
}
ZPev <- function(SRL) {
  TL <- (3.703*SRL)+225.536
  return(TL)
}
ZWhittyL <- function(SRL){
  TL <- SRL/0.24
  return(TL)
}
ZWhittyP <- function(SRL){
  TL <- 9.3627*(SRL^0.8559)
  return(TL)
}
ZBiskisLin <- function(SRL){
  TL <- 3.843*SRL+100.97
  return(TL)
}
ZBiskisP <- function(SRL){
  TL <- 4.4713*(SRL^0.9826)
  return(TL)
}

###Plot----
#define axes for print

ACurve<-function(){
  y_ticksa <- seq(0, 4000, by = 1000)  # Adjust the upper limit as needed
  plot(NULL, xlim = c(200, 800), ylim = c(500, 4000), yaxt = "n",
       ylab = '', xlab = '',
       main = "Anoxypristis cuspidata", font.main = 3,
       cex.main = 1.25, cex.axis = 1.2)
  axis(2, at = y_ticksa, labels = y_ticksa, cex.axis = 1.2)
  curve(AFariaL, add=TRUE, lwd=2, lty=2, col='orange')
  curve(AFariaP, add=TRUE, lwd=2, col='yellow')
  curve(APev, add=TRUE, lwd=2, lty=2, 
        col=rgb(0.2,0.4,0.1,0.7))
  curve(AWhittyL, add=TRUE, lwd=2, lty=2, col='dark blue')
  curve(AWhittyQ, add=TRUE, lwd=2, col='light blue')
  legend("bottomright",   # Position
        inset = 0.05, # Distance from the margin as a fraction of the plot region
        legend = c("Faria (2007) Linear", 
                   "Faria (2007) Power", 
                   "Peverell (2010)", 
                   "Whitty et al. (2014) Ratio", 
                   "Whitty et al. (2014)"),
        lty = c(2, 1, 2, 2, 1),
        col = c('orange', 
                'yellow', 
                rgb(0.2,0.4,0.1,0.7), 
                'darkblue',
                'lightblue'),
        theme(legend.text=element_text(size=12)))
  #curve(ABiskisL, add=TRUE, lwd=2, col='grey')
  #curve(ABiskisP, add=TRUE, lwd=2, col='black')
  #curve(ABiskisQ, add=TRUE, lwd=2, col='light grey') 
  }

CCurve<-function(){
  y_ticksc <- seq(0, 5000, by = 2000)  # Adjust the upper limit as needed
  plot(NULL, xlim = c(150, 800), ylim = c(750, 5000), yaxt = "n",
       ylab = '', xlab = '',
       main = "Pristis clavata", font.main = 3,
       cex.main = 1.25, cex.axis = 1.2)
  axis(2, at = y_ticksc, labels = y_ticksc, cex.axis = 1.2)
  curve(CFariaL, add = TRUE, col='orange', lwd=2, lty=2)
  curve(CFariaP, add=TRUE, lwd=2, col='yellow')
  curve(CPev, add=TRUE, lwd=2, lty =2, col=rgb(0.2,0.4,0.1,0.7))
  curve(CThor, add=TRUE, lwd=2, col='plum')
  curve(CWhittyL, add=TRUE, lwd=2, lty=2, col='dark blue')
  curve(CWhittyP, add=TRUE, lwd=2, col='light blue')
  # legend("bottomright",   # Position
  #        inset = 0.05, # Distance from the margin as a fraction of the plot region
  #        legend = c("Faria (2007) Linear", 
  #                   "Faria (2007) Power", 
  #                   "Peverell (2010)", 
  #                   "Thorburn et al. (2008)", 
  #                   "Whitty et al. (2014) Ratio", 
  #                   "Whitty et al. (2014)"),
  #        lty = c(2, 1, 2, 1, 2, 1),
  #        col = c('orange', 'yellow', rgb(0.2,0.4,0.1,0.7), 'plum', 'darkblue', 'lightblue'),
  #        theme(legend.text=element_text(size=12)))
  #curve(CBiskisL, add=TRUE, lwd=2, col='grey')
  #curve(CBiskisP, add=TRUE, lwd=2, col='black')
}

PCurve<-function(){
  y_ticksp <- seq(0, 6000, by = 2000)  # Adjust the upper limit as needed
  plot(NULL, xlim = c(150, 1200), ylim = c(750, 6000), yaxt = "n",
       ylab = '', xlab = '',
       main = "Pristis pristis", font.main = 3,
       cex.main = 1.25, cex.axis = 1.2)
  axis(2, at = y_ticksp, labels = y_ticksp, cex.axis = 1.2)
  curve(PFariaL, add =TRUE, lwd=2, lty=2, col='orange')
  curve(PFariaP, add=TRUE, lwd=2, col='yellow')
  curve(PMorg, add=TRUE, lwd=2, lty=2, col='red')
  curve(PPev, add=TRUE, col=rgb(0.2,0.4,0.1,0.7), 
        lwd=2, lty=2)
  curve(PThorF, add=TRUE, lwd=2, col='magenta')
  curve(PThorM, add=TRUE, lwd=2, col='purple')
  curve(PWhittyL, add=TRUE, lwd=2, lty=2, col='dark blue')
  curve(PWhittyP, add=TRUE, lwd=2, col='light blue')
  # legend("bottomright",   # Position
  #        inset = 0.05,
  #        legend = c("Faria (2007) Linear", 
  #                   "Faria (2007) Power",
  #                   "Morgan et al. (2011)", 
  #                   "Peverell (2010)", 
  #                   "Thorburn et al. (2007) Female", 
  #                   "Thorburn et al. (2007) Male", 
  #                   "Whitty et al. (2014) Ratio", 
  #                   "Whitty et al. (2014)"),
  #        lty = c(2, 1, 2, 2, 1, 1, 2, 1),
  #        col = c('orange', 'yellow', 'red', rgb(0.2,0.4,0.1,0.7), 'magenta', 'purple', 'darkblue', 'lightblue'),
  #        theme(legend.text=element_text(size=12)))
  #curve(CBiskisL, add=TRUE, lwd=2, col='grey')
  #curve(CBiskisP, add=TRUE, lwd=2, col='black')
}

ZCurve<-function(){
  y_ticksz <- seq(0, 7000, by = 2000)  # Adjust the upper limit as needed
  plot(NULL, xlim = c(250, 1800), ylim = c(750, 7000), yaxt = "n",
       ylab = '', xlab = '',
       main = "Pristis zijsron", font.main = 3,
       cex.main = 1.25, cex.axis = 1.2)
  axis(2, at = y_ticksz, labels = y_ticksz, cex.axis = 1.2)
  curve(ZFariaL, add = TRUE, lwd=2, lty=2, col='orange')
  curve(ZFariaP, add=TRUE, lwd=2, col='yellow')
  curve(ZLearF, add=TRUE, lwd=2, col='darkgreen')
  curve(ZLearM, add=TRUE, lwd=2, col='lightgreen')
  curve(ZPev, add=TRUE, 
        col=rgb(0.2,0.4,0.1,0.7), lwd=2, lty=2)
  curve(ZWhittyL, add=TRUE, lwd=2, lty=2, col='darkblue')
  curve(ZWhittyP, add=TRUE, lwd=2, col='lightblue')
  # legend("bottomright",   # Position
  #        inset = 0.05, # Distance from the margin as a fraction of the plot region
  #        legend = c("Faria (2007) Linear", 
  #                   "Faria (2007) Power",
  #                   "Lear et al. (2023) Female", 
  #                   "Lear et al. (2023) Male", 
  #                   "Peverell (2010)", 
  #                   "Whitty et al. (2014) Ratio", 
  #                   "Whitty et al. (2014)"),
  #        lty = c(2, 2, 1, 1, 1, 1, 2),
  #        col =  c('orange', 'yellow', 'darkgreen', 'lightgreen', rgb(0.2,0.4,0.1,0.7), 'darkblue', 'lightblue'),
  #        theme(legend.text=element_text(size=12)))
  #curve(CBiskisL, add=TRUE, lwd=2, col='grey')
  #curve(CBiskisP, add=TRUE, lwd=2, col='black')
}

####Individual----
ACurve()
CCurve()
PCurve()
ZCurve()

library(ggplot2)
library(cowplot)
library(grid)
library(gridExtra)

####Cowplot----
PubsC <- cowplot::plot_grid(
  ACurve,
  CCurve,
  PCurve,
  ZCurve,
  labels = c('a)', 'b)', 'c)', 'd)'),
  label_size= 15,
  label_fontface = "plain",
  hjust = -6,
  #vjust = 1,
  ncol = 2
  )

x.grob <- textGrob("Standard Rostrum Length (SRL) (mm)", 
                   gp = gpar(fontsize=16),
                   vjust = -1)

y.grob <- textGrob("Total Length (TL) (mm)", 
                   gp = gpar(fontsize=16),
                   rot = 90,
                   vjust = .5)

BoxCurves <- grid.arrange(arrangeGrob(PubsC), left = y.grob, bottom = x.grob)

ggsave(
  "SpecCurves.tiff",
  plot = BoxCurves,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 12,
  height = 11,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

####Biskis----

BA <-function(){
  curve(ZBiskisLin, from=200, to=1800, 
        xlab="Standard Rostrum Length (mm)", 
        ylab="Total Length (mm)", 
        col='dark green',
        lwd=2, lty=1, 
        main="Models")
  curve(CBiskisP, from=200, to=1000, add=TRUE, lwd=2, lty=2, col='orange')
  curve(PBiskisL, from=200, to=1400, add=TRUE, lwd=2, lty=1, col='dark blue')
  curve(ABiskisL, from=200, to=1000, add=TRUE, lwd=2, col='red')
  text(840, 6500, expression(TL == 2.759*(SRL^1.1215)), cex = 0.65)
  text(900, 4500, expression(TL == 4.017*SRL + 167.41), cex = 0.65)
  text(1300, 6000, expression(TL == 4.613*SRL - 87.58), cex = 0.65)
  text(1500, 6800, expression(TL == 3.830*SRL + 126.97), cex = 0.65)
  legend("bottomright",   # Position
         inset = 0.05, # Distance from the margin as a fraction of the plot region
         legend = c("A. cuspidata", "P. clavata", "P. pristis", "P. zijsron"),
         lty = c(1, 2, 1, 1),
         col = c('red', 'orange', 'dark blue', 'dark green'),
         theme(legend.text=element_text(size=8)))
}

BA()


#All Species----
##Basic Stats----
sumtable(AllS, group = "Species", group.long=T)
sumtable(AllS, group = "Source", group.long=T)
sumtable(AllS)

##Error Analysis----

###Normality----
ggqqplot(ImgJ$ErrTTC)
ggdensity(ImgJ$ErrTTC,
          main = "Error TTC",
          xlab = "Percent Error")
shapiro.test(ImgJ$ErrTTC) #normal

ggqqplot(ImgJ$ErrTL)
ggdensity(ImgJ$ErrTL,
          main = "Error TL",
          xlab = "Percent Error")
shapiro.test(ImgJ$ErrTL) #normal

ggqqplot(ImgJ$ErrTRL)
ggdensity(ImgJ$ErrTRL,
          main = "Error TRL",
          xlab = "Percent Error")
shapiro.test(ImgJ$ErrTRL) #almost normal

ggqqplot(ImgJ$ErrSRL)
ggdensity(ImgJ$ErrSRL,
          main = "Error SRL",
          xlab = "Percent Error")
shapiro.test(ImgJ$ErrSRL) #not normal

ggqqplot(ImgJ$ErrSRW)
ggdensity(ImgJ$ErrSRW,
          main = "Error SRW",
          xlab = "Percent Error")
shapiro.test(ImgJ$ErrSRW) #almost normal

###Means----

dfI2 %>%
  ggplot(aes(x=Error, fill=Meas))+
  geom_density(alpha=0.5)+
  #facet_wrap(~Age_Class)+
  scale_x_log10()+
  #theme(legend.position="none")+
  labs(x="Percent Error")

Err1 <- dfI2 %>%
  group_by(Meas) %>%
  summarise(
    count = n(),
    mean = mean(NE, na.rm = TRUE),
    sd = sd(NE, na.rm = TRUE)
  ) %>%
  ungroup()
print(Err1, n = Inf)

###t-tests----

dfI %>%  #ttest - calc vs true SRL
  do(tidy(t.test(.$NETRL,
                      .$NESRL,
                      mu = 0,
                      alt = "two.sided",
                      paired = FALSE,
                      conf.level = 0.99)))

dfI %>%  #ttest - calc vs true SRL
  do(tidy(t.test(.$M_TL,
                      .$J_TL,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      conf.level = 0.99)))

dfI %>%  #ttest - calc vs true SRL
  do(tidy(t.test(.$M_TTC,
                      .$J.TTC,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      conf.level = 0.99)))

dfI %>%  #ttest - calc vs true SRL
  do(tidy(t.test(.$M_TRL,
                      .$J_TRL,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      conf.level = 0.99)))

dfI %>%  #ttest - calc vs true SRL
  do(tidy(t.test(.$M_SRL,
                      .$J_SRL,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      exact = FALSE,
                      conf.level = 0.99)))
#By Species
dfI %>%  #ttest - calc vs true SRL
  group_by(Species)%>%
  do(tidy(wilcox.test(.$M_TL,
                 .$J_TL,
                 mu = 0,
                 alt = "two.sided",
                 paired = TRUE,
                 exact = FALSE,
                 conf.level = 0.99)))%>%
  ungroup()

dfI %>%  #ttest - calc vs true SRL
  group_by(Species)%>%
  do(tidy(wilcox.test(.$M_TTC,
                      .$J.TTC,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      exact = FALSE,
                      conf.level = 0.99)))%>%
  ungroup()

dfI %>%  #ttest - calc vs true SRL
  group_by(Type)%>%
  do(tidy(wilcox.test(.$M_TRL,
                      .$J_TRL,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      exact = FALSE,
                      conf.level = 0.99)))%>%
  ungroup()

dfI %>%  #ttest - calc vs true SRL
  group_by(Species)%>%
  do(tidy(wilcox.test(.$M_SRL,
                      .$J_SRL,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      exact = FALSE,
                      conf.level = 0.99)))%>%
  ungroup()

###Associations----

t.test(formula = ErrTTC ~ Scale, data = dfI)
summary(aov(formula = ErrTTC ~ Species, data = dfI))
summary(aov(formula = ErrTTC ~ Type, data = dfI))
summary(aov(formula = ErrTTC ~ Researcher, data = dfI))

t.test(formula = ErrTL ~ Scale, data = dfI)
summary(aov(formula = ErrTL ~ Species, data = dfI))
summary(aov(formula = ErrTL ~ Researcher, data = dfI))

t.test(formula = ErrTRL ~ Scale, data = dfI)
summary(aov(formula = ErrTRL ~ Species, data = dfI))
summary(aov(formula = ErrTRL ~ Type, data = dfI))
summary(aov(formula = ErrTRL ~ Researcher, data = dfI))

t.test(formula = ErrSRL ~ Scale, data = dfI)
summary(aov(formula = ErrSRL ~ Species, data = dfI))
summary(aov(formula = ErrSRL ~ Type, data = dfI))
summary(aov(formula = ErrSRL ~ Researcher, data = dfI))

t.test(formula = ErrSRW ~ Scale, data = dfI)
summary(aov(formula = ErrSRW ~ Species, data = dfI))
summary(aov(formula = ErrSRW ~ Type, data = dfI))
summary(aov(formula = ErrSRW ~ Researcher, data = dfI))

summary(aov(formula = NETL ~ Species, data = dfI))
summary(aov(formula = NETTC ~ Species, data = dfI))
summary(aov(formula = NETRL ~ Species, data = dfI))
summary(aov(formula = NESRL ~ Species, data = dfI))
summary(aov(formula = NESRW ~ Species, data = dfI))

summary(aov(formula = NE ~ Species, data = dfI2))
summary(aov(formula = NE ~ Scale, data = dfI2))
summary(aov(formula = NE ~ Meas + Species + Meas:Species, data = dfI1))

Err3 <- dfI1 %>%
  group_by(Meas, Scale) %>%
  summarise(
    count = n(),
    mean = mean(NE, na.rm = TRUE),
    sd = sd(NE, na.rm = TRUE)
  ) %>%
  ungroup()
print(Err3, n = Inf)

Err4 <- dfI1 %>%
  group_by(Species, Meas) %>%
  summarise(
    count = n(),
    mean = mean(NE, na.rm = TRUE),
    sd = sd(NE, na.rm = TRUE)
  ) %>%
  ungroup()
print(Err4, n = Inf)

Err5 <- dfI1 %>%
  group_by(Meas) %>%
  summarise(
    count = n(),
    mean = mean(NE, na.rm = TRUE),
    sd = sd(NE, na.rm = TRUE)
  ) %>%
  ungroup()
print(Err5, n = Inf)

Err6 <- dfI1 %>%
  group_by(Species) %>%
  summarise(
    count = n(),
    mean = mean(NE, na.rm = TRUE),
    sd = sd(NE, na.rm = TRUE)
  ) %>%
  ungroup()
print(Err6, n = Inf)

FinErr <- dfI1 %>%
  summarise(
    count = n(),
    mean = mean(NE, na.rm = TRUE),
    sd = sd(NE, na.rm = TRUE)
  ) 
FinErr

TotErr <- dfI1 %>%
  dplyr::summarise(
    count = n(),
    mean = mean(Error, na.rm = TRUE),
    sd = sd(Error, na.rm = TRUE)
  ) 
TotErr

###Plot----
ggplot(I2, aes(x=factor(Meas,
                        level = c('TL', 'TTC', 'TRL', 'SRL', 'SRW', 'TW')),
               y=Error)) + 
  geom_boxplot() +
 facet_grid(~ Species) +
  theme(legend.text = element_text(face = "italic")) +
  theme(axis.text = element_text(face = "italic")) +
  #scale_color_manual(values = c("blue4","gold2", "red3", "darkgreen")) +
  labs(x = 'Measurement',
       y = "Percent Error")


##Get Ratios----
library(tidyverse)
library(tidyr)
library(dplyr)

group_by(AllS, Species) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(LJR_SRL, na.rm = TRUE),
    sd = sd(LJR_SRL, na.rm = TRUE)
  ) %>%
  ungroup()

group_by(AllS, Species) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(SRL_TL, na.rm = TRUE),
    sd = sd(SRL_TL, na.rm = TRUE)
  ) %>%
  ungroup()

group_by(AllS, Species) %>%
  summarise(
    count = n(),
    mean = mean(SRL_TRL, na.rm = TRUE),
    sd = sd(SRL_TRL, na.rm = TRUE)
  ) %>%
  ungroup()

group_by(AllS, Species) %>%
  summarise(
    count = n(),
    mean = mean(TRL_TL, na.rm = TRUE),
    sd = sd(TRL_TL, na.rm = TRUE)
  ) %>%
  ungroup()

group_by(AllS, Species) %>%
  summarise(
    count = n(),
    mean = mean(LJR_TRL, na.rm = TRUE),
    sd = sd(LJR_TRL, na.rm = TRUE)
  ) %>%
  ungroup()

group_by(AllS, Species) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(SRL_TRL, na.rm = TRUE),
    sd = sd(SRL_TRL, na.rm = TRUE)
  ) %>%
  ungroup()

group_by(AllS, State) %>%
  summarise(
    count = n(),
    percent = n()*100/243,
  ) %>%
  ungroup()

group_by(AllS, Species) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(TL_curve, na.rm = TRUE),
    sd = sd(TL_curve, na.rm = TRUE),
    max = max(TL_curve, na.rm = TRUE),
    min = min(TL_curve, na.rm = TRUE)
  ) %>%
  ungroup()

group_by(AllS, Species) %>%
  summarise(
    count = n(),
    mean = mean(TL_stats, na.rm = TRUE),
    sd = sd(TL_stats, na.rm = TRUE),
    max = max(TL_stats, na.rm = TRUE),
    min = min(TL_stats, na.rm = TRUE)
  ) %>%
  ungroup()

###Test Var----
library(ggpubr)
library(rstatix)
library(car)

####LJR/SRL----
#####ALL----

ggqqplot(AllS$LJR_SRL)
ggdensity(AllS$LJR_SRL,
          main = "Density plot of LJR/SRL",
          xlab = "LJR/SRL")
shapiro.test(AllS$LJR_SRL) #not normal

#####By Spec----
AllS$Species <- as.factor(AllS$Species)
AllS$Age_Class <- as.factor(AllS$Age_Class)
leveneTest(LJR_SRL ~ Species, data = AllS) #unequal var
leveneTest(LJR_SRL ~ Age_Class, data = AllS) #unequal var
leveneTest(LJR_SRL ~ Species*Age_Class, data = AllS) #unequal var

df1 %>%
  ggplot(aes(x=LJR_SRL, fill=Species))+
  geom_density(alpha=0.5)+
  #facet_wrap(~Age_Class)+
  scale_x_log10()+
  #theme(legend.position="none")+
  labs(x="LJR/SRL Ratio")

df1 %>%
  group_by(Species) %>%
  shapiro_test(LJR_SRL) %>%
  ungroup()

####SRL/TL----
#####ALL----
ggqqplot(AllS$SRL_TL)
ggdensity(AllS$SRL_TL, 
          main = "Density plot of SRL_TL",
          xlab = "SRL_TL")
shapiro.test(AllS$SRL_TL) #normal

#####By Spec----
df1 %>%
  ggplot(aes(x=SRL_TL, fill=Species))+
  geom_density(alpha=0.5)+
  #facet_wrap(~Age_Class)+
  scale_x_log10()+
  #theme(legend.position="none")+
  labs(x="SRL/TL Ratio")

df1 %>%
  group_by(Species) %>%
  shapiro_test(SRL_TL) %>%
  ungroup()

leveneTest(SRL_TL ~ Species, data = AllS) #equal var
leveneTest(SRL_TL ~ Age_Class, data = AllS) #unequal var
leveneTest(SRL_TL ~ Species*Age_Class, data = AllS) #equal var

####SRL/TRL----
#####ALL----
ggqqplot(AllS$SRL_TRL)
ggdensity(AllS$SRL_TRL, 
          main = "Density plot of SRL_TRL",
          xlab = "SRL_TRL")
shapiro.test(AllS$SRL_TRL) #not normal

#####By Spec----
df1 %>%
  ggplot(aes(x=SRL_TRL, fill=Species))+
  geom_density(alpha=0.5)+
  #facet_wrap(~Age_Class)+
  scale_x_log10()+
  #theme(legend.position="none")+
  labs(x="SRL/TRL Ratio")

df1 %>%
  group_by(Species) %>%
  shapiro_test(SRL_TRL) %>%
  ungroup()

leveneTest(SRL_TRL ~ Species, data = AllS) #unequal var
leveneTest(SRL_TRL ~ Age_Class, data = AllS) #unequal var
leveneTest(SRL_TRL ~ Species*Age_Class, data = AllS) #unequal var

####TL----
#####ALL----
ggqqplot(AllS$TL_curve)
ggdensity(AllS$TL_curve, 
          main = "Density plot of TL",
          xlab = "TL")
shapiro.test(AllS$TL_curve) #not normal

#####By Spec----
df1 %>%
  ggplot(aes(x=TL_curve, fill=Species))+
  geom_density(alpha=0.5)+
  #facet_wrap(~Age_Class)+
  scale_x_log10()+
  #theme(legend.position="none")+
  labs(x="TL")

df1 %>%
  group_by(Species) %>%
  shapiro_test(TL_curve) %>%
  ungroup()

leveneTest(TL_curve ~ Species, data = AllS) #unequal var
leveneTest(TL_curve ~ Age_Class, data = AllS) #unequal var
leveneTest(TL_curve ~ Species*Age_Class, data = AllS) #equal var

##Test Ratios----

###Reliability----

#Anoxy
wilcox.test(formula = LJR_SRL~ TL_Meas, data = dfA)
wilcox.test(formula = LJR_SRL~ TL_Est, data = dfA)
wilcox.test(formula = LJR_SRL~ Rost_Real, data = dfA)
wilcox.test(formula = LJR_SRL~ Multiple_Planes, data = dfA)
wilcox.test(formula = LJR_SRL~ TL_Real, data = dfA)

wilcox.test(formula = SRL_TRL~ TL_Meas, data = dfA)
wilcox.test(formula = SRL_TRL~ TL_Est, data = dfA)
wilcox.test(formula = SRL_TRL~ Rost_Real, data = dfA)
wilcox.test(formula = SRL_TRL~ Multiple_Planes, data = dfA)
wilcox.test(formula = SRL_TRL~ TL_Real, data = dfA)

wilcox.test(formula = SRL_TL~ TL_Meas, data = dfA)
wilcox.test(formula = SRL_TL~ TL_Est, data = dfA)
wilcox.test(formula = SRL_TL~ Rost_Real, data = dfA)
wilcox.test(formula = SRL_TL~ Multiple_Planes, data = dfA)
wilcox.test(formula = SRL_TL~ TL_Real, data = dfA)

kruskal.test(LJR_SRL ~ State, data = AC)
kruskal.test(LJR_SRL ~ Source, data = AC)
kruskal.test(SRL_TRL ~ State, data = AC)
kruskal.test(SRL_TRL ~ Source, data = AC)
kruskal.test(SRL_TL ~ State, data = AC)
kruskal.test(SRL_TL ~ Source, data = AC)

#Clavata 

wilcox.test(formula = LJR_SRL~ TL_Meas, data = dfC)
wilcox.test(formula = LJR_SRL~ TL_Est, data = dfC)
wilcox.test(formula = LJR_SRL~ Rost_Real, data = dfC)
wilcox.test(formula = LJR_SRL~ Multiple_Planes, data = dfC)
wilcox.test(formula = LJR_SRL~ TL_Real, data = dfC)

wilcox.test(formula = SRL_TRL~ TL_Meas, data = dfC)
wilcox.test(formula = SRL_TRL~ TL_Est, data = dfC)
wilcox.test(formula = SRL_TRL~ Rost_Real, data = dfC)
wilcox.test(formula = SRL_TRL~ Multiple_Planes, data = dfC)
wilcox.test(formula = SRL_TRL~ TL_Real, data = dfC)

wilcox.test(formula = SRL_TL~ TL_Meas, data = dfC)
wilcox.test(formula = SRL_TL~ TL_Est, data = dfC)
wilcox.test(formula = SRL_TL~ Rost_Real, data = dfC)
wilcox.test(formula = SRL_TL~ Multiple_Planes, data = dfC)
wilcox.test(formula = SRL_TL~ TL_Real, data = dfC)

kruskal.test(LJR_SRL ~ State, data = PC)
kruskal.test(LJR_SRL ~ Source, data = PC)
kruskal.test(SRL_TRL ~ State, data = PC)
kruskal.test(SRL_TRL ~ Source, data = PC)
kruskal.test(SRL_TL ~ State, data = PC)
kruskal.test(SRL_TL ~ Source, data = PC)

#Pristis

wilcox.test(formula = LJR_SRL~ TL_Meas, data = dfP)
wilcox.test(formula = LJR_SRL~ TL_Est, data = dfP)
wilcox.test(formula = LJR_SRL~ Rost_Real, data = dfP)
wilcox.test(formula = LJR_SRL~ Multiple_Planes, data = dfP)
wilcox.test(formula = LJR_SRL~ TL_Real, data = dfP)

wilcox.test(formula = SRL_TRL~ TL_Meas, data = dfP)
wilcox.test(formula = SRL_TRL~ TL_Est, data = dfP)
wilcox.test(formula = SRL_TRL~ Rost_Real, data = dfP)
wilcox.test(formula = SRL_TRL~ Multiple_Planes, data = dfP)
wilcox.test(formula = SRL_TRL~ TL_Real, data = dfP)

wilcox.test(formula = SRL_TL~ TL_Meas, data = dfP)
wilcox.test(formula = SRL_TL~ TL_Est, data = dfP)
wilcox.test(formula = SRL_TL~ Rost_Real, data = dfP)
wilcox.test(formula = SRL_TL~ Multiple_Planes, data = dfP)
wilcox.test(formula = SRL_TL~ TL_Real, data = dfP)

kruskal.test(LJR_SRL ~ State, data = AllS)
kruskal.test(LJR_SRL ~ Source, data = AllS)
kruskal.test(SRL_TRL ~ State, data = AllS)
kruskal.test(SRL_TRL ~ Source, data = AllS)
kruskal.test(SRL_TL ~ State, data = AllS)
kruskal.test(SRL_TL ~ Source, data = AllS)

#Zijsron
wilcox.test(formula = LJR_SRL~ TL_Meas, data = dfZ)
wilcox.test(formula = LJR_SRL~ TL_Est, data = dfZ)
wilcox.test(formula = LJR_SRL~ Rost_Real, data = dfZ)
wilcox.test(formula = LJR_SRL~ Multiple_Planes, data = dfZ)
wilcox.test(formula = LJR_SRL~ TL_Real, data = dfZ)

wilcox.test(formula = SRL_TRL~ TL_Meas, data = dfZ)
wilcox.test(formula = SRL_TRL~ TL_Est, data = dfZ)
wilcox.test(formula = SRL_TRL~ Rost_Real, data = dfZ)
wilcox.test(formula = SRL_TRL~ Multiple_Planes, data = dfZ)
wilcox.test(formula = SRL_TRL~ TL_Real, data = dfZ)

wilcox.test(formula = SRL_TL~ TL_Meas, data = dfZ)
wilcox.test(formula = SRL_TL~ TL_Est, data = dfZ)
wilcox.test(formula = SRL_TL~ Rost_Real, data = dfZ)
wilcox.test(formula = SRL_TL~ Multiple_Planes, data = dfZ)
wilcox.test(formula = SRL_TL~ TL_Real, data = dfZ)

kruskal.test(LJR_SRL ~ State, data = AllS)
kruskal.test(LJR_SRL ~ Source, data = AllS)
kruskal.test(SRL_TRL ~ State, data = AllS)
kruskal.test(SRL_TRL ~ Source, data = AllS)
kruskal.test(SRL_TL ~ State, data = AllS)
kruskal.test(SRL_TL ~ Source, data = AllS)

###Categorical----
aov0 <- aov(LJR_SRL ~ Species, data = AllS) #1Way
summary(aov0)
aov1 <- aov(LJR_TRL ~ Species, data = AllS) #1Way
summary(aov1)
aov2 <- aov(SRL_TRL ~ Species, data = AllS) #1Way
summary(aov2)
aov3 <- aov(SRL_TL ~ Species, data = AllS) #1Way
summary(aov3)

wilcox.test(formula = SRL_TRL ~ Maturity, data = AllS)
wilcox.test(formula = SRL_TL ~ Maturity, data = AllS)
wilcox.test(formula = LJR_SRL ~ Maturity, data = AllS)

kruskal.test(LJR_SRL ~ Species, data = AllS)
kruskal.test(SRL_TRL ~ Species, data = AllS)
kruskal.test(SRL_TL ~ Species, data = AllS)
kruskal.test(LJR_TRL ~ Species, data = AllS)

#2Ways
aov2w0 <- aov(LJR_SRL~ Species + Maturity + Species:Maturity, data = AllS) #2Way
summary(aov2w0)
aov2w1 <- aov(SRL_TL ~ Species + Maturity + Species:Maturity, data = AllS) #2Way
summary(aov2w1)
aov2w2 <- aov(SRL_TRL ~ Species + Maturity + Species:Maturity, data = df1) #2Way
summary(aov2w2)
aov2w3 <- aov(SRL_TL ~ Species + Age_Class + Species:Age_Class, data = AllS) #2Way
summary(aov2w3)
aov2w4 <- aov(SRL_TRL ~ Species + Age_Class + Species:Age_Class, data = AllS) #2Way
summary(aov2w4)

###Correlations----

#tests
model_spec <- lm(LJR_SRL ~ Species, data = AllS)
summary(model_spec)

#better for individual by species
cont <- as.vector(which(sapply(AllS, is.numeric)))
cor(AllS$LJR_SRL, AllS[,cont], method = "spearman", use = "pairwise.complete.obs")
cor(AllS$SRL_TRL, AllS[,cont], method = "spearman", use = "pairwise.complete.obs")
cor(AllS$SRL_TL, AllS[,cont], method = "spearman", use = "pairwise.complete.obs")

#Split by desired var
RatTest <- c("Species",	"Age_Class",	"Maturity",	"TL_rep", "TL_curve",	"LJR_SRL", "LJR_TRL",	"SRL_TRL",	"SRL_TL",	"TRL_TL")
AllS2 <- AllS[RatTest]
AllS2$Species <- factor(AllS2$Species)
AllS2$Maturity <- factor(AllS2$Maturity)
AllS2$Age_Class <- factor(AllS2$Age_Class)

require(plyr)
func <- function(AllS2)
{
  return(data.frame(COR = cor(AllS2$LJR_SRL, AllS2[,cont], use = "complete.obs")))
}
ddply(AllS2, .(Species, Maturity), func)
ddply(AllS2, .(Species), func)

func2 <- function(AllS2)
{
  return(data.frame(COR = cor(AllS2$SRL_TRL, AllS2[,cont], use = "complete.obs")))
}
ddply(AllS2, .(Species, Maturity), func2)
ddply(AllS2, .(Species), func2)

func3 <- function(AllS2)
{
  return(data.frame(COR = cor(AllS2$SRL_TL, AllS2[,cont], use = "complete.obs")))
}
ddply(AllS2, .(Species, Maturity), func3)
ddply(AllS2, .(Species), func3)

###Density Plots----
library(ggExtra)

count <- 0
breaks_fun <- function(x) {
  count <<- count + 1L
  switch(
    count,
    c(1000, 2000, 3000),
    c(1000, 2000, 3000),
    c(2000, 4000, 6000),
    c(1000, 3000, 6000)
  )
}

StateSRL<- ggplot(AllS, aes(x=TL_curve, y=SRL_TL, color=State)) + 
   geom_point() +
  # scale_x_continuous(breaks = breaks_fun, limits = c(0, NA)) + 
   facet_grid(rows = vars(Species),
             scales = "free_x")
  labs(x= "TL", y = "SRL/TL",
       title= "SRL/TL Ratio vs Size by Location and Species") +
   geom_smooth(method=lm) +
  theme(strip.text.y = element_text(face = "italic")) +
  scale_color_manual(values = c( "grey", "skyblue","#CC6600", "maroon", "gold1"))
StateSRL

#Individual Spec

StateSRLA<- ggplot(AC, aes(x=TL_curve, y=SRL_TL, color=State)) + 
  geom_point() +
  labs(x= "TL", y = "SRL/TL") +
  theme(legend.position = "none") +
  geom_smooth(method=lm) +
  scale_color_manual(values = c("#CC6600", "maroon", "gold1"))

StateSRLC<- ggplot(PC, aes(x=TL_ImgJ, y=SRL_TL, color=State)) + 
  geom_point() +
  theme(legend.position = "none")+
  labs(x= "TL", y = "SRL/TL") +
  geom_smooth(method=lm) +
  scale_color_manual(values = c("grey", "#CC6600", "maroon", "gold1"))

StateSRLP<- ggplot(PP, aes(x=TL_curve, y=SRL_TL, color=State)) + 
  geom_point() +
  labs(x= "TL", y = "SRL/TL") +
  theme(legend.position = "none") +
  geom_smooth(method=lm) +
  scale_color_manual(values = c("#CC6600", "maroon", "gold1"))

StateSRLZ<- ggplot(PZ, aes(x=TL_curve, y=SRL_TL, color=State)) + 
  geom_point() +
  theme(legend.position = "none") +
  labs(x= "TL", y = "SRL/TL") +
  geom_smooth(method=lm) +
  scale_color_manual(values = c("skyblue","#CC6600", "maroon", "gold1"))

StatewDA <-  ggMarginal(StateSRLA, type = "density",
                       groupColour = TRUE,
                       margins = 'y')

StatewDC <-  ggMarginal(StateSRLC, type = "density",
                        groupColour = TRUE,
                        margins = 'y')

StatewDP <-  ggMarginal(StateSRLP, type = "density",
                        groupColour = TRUE,
                        margins = 'y')

StatewDZ <-  ggMarginal(StateSRLZ, type = "density",
                        groupColour = TRUE,
                        margins = 'y')

legend <- get_legend(StateSRLZ +
                     theme(legend.position = "bottom"))

StateTop<-plot_grid(StatewDA,
                    StatewDC,
                    ncol=2)
StateBot<-plot_grid(StatewDP,
                    StatewDZ,
                    ncol=2)

cowplot::plot_grid(StateTop,
                   StateBot,
                   legend,
                   nrow = 3,
                   rel_heights = c(1,1,0.1))

ggsave(
  "StateDiff.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8,
  height = 6,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

###Boxplots: Species-Size----

ggplot(AllS, aes(x=TL_curve, y=LJR_SRL, color=Species)) + 
  geom_point()+
  labs(x= "TL", y = "LJR/SRL", 
       title= "LJR/SRL Ratio vs Size by Species") +
  geom_smooth(method=lm) +
  theme(legend.text = element_text(face = "italic")) +
  scale_color_manual(values = c("blue4","gold2", "red3", "darkgreen")) 

SRLTL <- ggplot(AllS, aes(x=TL_curve, y=SRL_TL, color=Species, shape = Species)) + 
  geom_point() +
  labs(x= "TL (mm)", y = "SRL/TL") +
  geom_smooth(method=lm) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.text = element_text(face = "italic"),
        legend.position = "none") +
  scale_shape_manual(values = c(4, 15, 16, 17)) +
  scale_color_manual(values = c("blue4","gold2", "red3", "darkgreen")) 
SRLTL

ggsave(
  "SRLTL.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 6,
  height = 4,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

install.packages("magick")
library(magick)
library(png)

ALg <- readPNG("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/USC/Papers/2. Morpho Paper/AnoxyLg.png")

SRLTRL <- ggplot(AllS, aes(x=TL_curve, y=SRL_TRL, color=Species, shape = Species)) + 
  geom_point()+
  labs(x= "TL (mm)", y = "SRL/TRL") +
  geom_smooth(method=lm) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(face = "italic", size = 13),
        legend.position = c(0.75, 0.24)) +
  scale_shape_manual(values = c(4, 15, 16, 17)) +
  scale_color_manual(values = c("blue4","gold2", "red3", "darkgreen")) 
SRLTRL

ggsave(
  "SRLTRL.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 4,
  height = 3,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

RatiosPair <- cowplot::plot_grid(SRLTL, SRLTRL,
                              labels = "", ncol = 2)
RatiosPair

ggsave(
  "RatiosPost.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 6,
  height = 4,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

#DensityPlot
df1 %>%
  ggplot(aes(x=LJR_SRL, fill=Species))+
  geom_density(alpha=0.5)+
  #facet_wrap(~Age_Class)+
  theme(legend.text = element_text(face = "italic")) +
  theme(axis.text = element_text(face = "italic")) +
  scale_fill_manual(values = c("blue4","gold2", "red3", "darkgreen")) +
  labs(x = "LJR/SRL")

#DensityPlot
df1 %>%
  ggplot(aes(x=SRL_TRL, fill=Species))+
  geom_density(alpha=0.5)+
  #facet_wrap(~Age_Class)+
  theme(legend.text = element_text(face = "italic")) +
  theme(axis.text = element_text(face = "italic")) +
  scale_fill_manual(values = c("blue4","gold2", "red3", "darkgreen")) +
  labs(x = "SRL/TRL", y = "Density")

#DensityPlot
df1 %>%
  ggplot(aes(x=SRL_TL, fill=Species))+
  geom_density(alpha=0.5)+
  #facet_wrap(~Age_Class)+
  theme(legend.text = element_text(face = "italic")) +
  theme(axis.text = element_text(face = "italic")) +
  scale_fill_manual(values = c("blue4","gold2", "red3", "darkgreen")) +
  labs(x = "SRL/TL", y = "Density")

ggplot(AllS, aes(x=Species, y=LJR_SRL, color=Species)) + 
  geom_boxplot() +
  facet_grid(~ Maturity) +
  theme(legend.text = element_text(face = "italic")) +
  theme(axis.text = element_text(face = "italic")) +
  scale_color_manual(values = c("blue4","gold2", "red3", "darkgreen")) +
  labs(y = "LJR/SRL")

ggsave(
  "LJRSRL.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

ggplot(AllS, aes(x=Species, y=SRL_TL, color=Species)) + 
  geom_boxplot() +
  facet_grid(~ Maturity) +
  #coord_flip() +
  labs(y = "SRL/TL") +
  theme(legend.text = element_text(face = "italic")) +
  theme(axis.text = element_text(face = "italic")) +
  scale_color_manual(values = c("blue4","gold2", "red3", "darkgreen")) +
  labs(y = "SRL/TL")

ggplot(AllS, aes(x=Species, y=SRL_TRL, color=Species)) + 
  geom_boxplot() +
  facet_grid(~ Maturity) +
  #coord_flip() +
  labs(y = "SRL/TRL")+
  theme(legend.text = element_text(face = "italic")) +
  theme(axis.text = element_text(face = "italic")) +
  scale_color_manual(values = c("blue4","gold2", "red3", "darkgreen")) +
  labs(y = "SRL/TRL")

##Isometry----

#SRL

IsoS<- ggplot(df1, 
       aes(x=log(TL_curve), y=log(SRL), color = Species)) + 
  geom_point() +
  stat_smooth(method="lm") + 
  stat_regline_equation(label.x = 6.5,
                        label.y = 7, aes(label = ..eq.label..)) +
  stat_regline_equation(label.x = 6.5,
                        label.y = 6.7, aes(label = ..rr.label..)) +
  scale_x_continuous(limits = c(6,9)) +
  scale_y_continuous(limits = c(4.5,7.5)) +
  facet_grid(~ Species) +
  #coord_flip() +
  labs(y = "log SRL", x = 'log TL')+
  theme(strip.text = element_text(face = "italic", size = 12), 
        axis.text = element_text(face = "italic", size = 11),
        axis.title = element_text(size = 12),
        legend.position = "none") +
  scale_color_manual(values = c("blue4","gold2", "red3", "darkgreen")) 

IsoS

IsoT<- ggplot(df1, 
       aes(x=log(TL_curve), y=log(TRL), color = Species)) + 
  geom_point() +
  stat_smooth(method="lm") + 
  stat_regline_equation(label.x = 6.5,
                        label.y = 7, aes(label = ..eq.label..)) +
  stat_regline_equation(label.x = 6.5,
                        label.y = 6.7, aes(label = ..rr.label..)) +
  scale_x_continuous(limits = c(6,9)) +
  scale_y_continuous(limits = c(4.5,7.5)) +
  facet_grid(~ Species) +
  #coord_flip() +
  labs(y = "log TRL", x = 'log TL')+
  theme(strip.text = element_text(face = "italic", size = 12), 
        axis.text = element_text(face = "italic", size = 11),
        axis.title = element_text(size = 12),
        legend.position = "none") +
  scale_color_manual(values = c("blue4","gold2", "red3", "darkgreen")) 

IsoL <- ggplot(df1, 
       aes(x=log(TL_curve), y=log(LJR), color = Species)) + 
  geom_point() +
  stat_smooth(method="lm") + 
  stat_regline_equation(label.x = 6.5,
                        label.y = 7, aes(label = ..eq.label..)) +
  stat_regline_equation(label.x = 6.5,
                        label.y = 6.7, aes(label = ..rr.label..)) +
  scale_x_continuous(limits = c(6,9)) +
  scale_y_continuous(limits = c(4.5,7.5)) +
  facet_grid(~ Species) +
  #coord_flip() +
  labs(y = "log LJR", x = 'log TL')+
  theme(strip.text = element_text(face = "italic", size = 12), 
        axis.text = element_text(face = "italic", size = 11),
        axis.title = element_text(size = 12),
        legend.position = "none") +
  scale_color_manual(values = c("blue4","gold2", "red3", "darkgreen")) 

IsoAllo <- cowplot::plot_grid(IsoS + theme(axis.title.x = element_blank()),
                              IsoT + theme(axis.title.x = element_blank()),
                              labels = c('a)',
                                         'b)',
                                         'c)',
                                         'd)'),
                              label_fontface = 'plain',
                                         ncol = 1)
IsoAllo

ggsave(
  "IsoAllo.Tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 10,
  height = 5,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

IsoAllo2 <- cowplot::plot_grid(IsoS + theme(axis.title.x = element_blank()),
                              IsoT,
                              labels = c('', ''), ncol = 1)

IsoAllo2

#TotalRatio

plots <- align_plots(SRLTRL + theme(plot.title = element_blank(),
                                   legend.position = "none"), 
                     IsoS + theme(axis.title.x = element_blank()),
                     IsoT, 
                     align = 'v', axis = 'l')

topIA <- plot_grid(plots[[1]], 
                   SRLTL + theme(plot.title = element_blank(), 
                                            legend.position = "none"),
                   labels = "AUTO", ncol = 2)

cowplot::plot_grid(topIA, 
                   plots[[2]],
                   plots[[3]],
                   labels = c('','C', 'D'), nrow = 3, 
                   rel_heights = c(0.9, 0.5, 0.5))

ggsave(
  "Ratios.Tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 10,
  height = 8,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)


#Anoxypristis----
##Test for Distribution----
library(car)
library(ggpubr)

###Normality----
####Ratios----
ggqqplot(AC$LJR_SRL)
ggdensity(AC$LJR_SRL,
          main = "Density plot of LJR/SRL",
          xlab = "LJR/SRL")
shapiro.test(AC$LJR_SRL) #pretty normal

ggqqplot(AC$SRL_TL)
ggdensity(AC$SRL_TL,
          main = "Density plot of SRL/TL",
          xlab = "SRL_TL")
shapiro.test(AC$SRL_TL) #pretty normal

ggqqplot(AC$SRL_TRL)
ggdensity(AC$SRL_TRL,
          main = "Density plot of SRL/TRL",
          xlab = "SRL_TL")
shapiro.test(AC$SRL_TRL) #pretty normal

ggqqplot(AC$TL_curve)
ggdensity(AC$TL_curve,
          main = "Density plot of TL",
          xlab = "TL")
shapiro.test(AC$TL_curve) #not normal

dfA %>%
  ggplot(aes(x=LJR_SRL, fill=Age_Class))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="LJR/SRL Ratio")

dfA %>%
  group_by(Age_Class) %>%
  shapiro_test(LJR_SRL) %>%
  ungroup()

dfA %>%
  ggplot(aes(x=SRL_TL, fill=Age_Class))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="SRL_TL Ratio")

dfA %>%
  group_by(Age_Class) %>%
  shapiro_test(SRL_TL) %>%
  ungroup()

dfA %>%
  ggplot(aes(x=TL_curve, fill=Age_Class))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="TL")

dfA %>%
  group_by(Age_Class) %>%
  shapiro_test(TL_curve) %>%
  ungroup()

dfA %>%
  ggplot(aes(x=SRL_TRL, fill=Age_Class))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="SRL/TRL")

dfA %>%
  group_by(Age_Class) %>%
  shapiro_test(SRL_TRL) %>%
  ungroup()

####Residuals----
ggqqplot(Ares$Residuals_SRL/1000)
ggdensity(Ares$Residuals_SRL/1000,
          main = "Density plot of Residuals",
          xlab = "Residuals - SRL")
shapiro.test(Ares$Residuals_SRL) #not normal

ggqqplot(Ares$SSq_SRL)
ggdensity(Ares$SSq_SRL,
          main = "Density plot of Residuals",
          xlab = "Residuals - SRL")
shapiro.test(Ares$SSq_SRL) #not normal

ggqqplot(Ares$NetRes_S)
ggdensity(Ares$NetRes_S,
          main = "Density plot of Residuals",
          xlab = "Residuals - SRL")
shapiro.test(Ares$NetRes_S) #not normal

ggqqplot(Ares$logResS)
ggdensity(Ares$logResS,
          main = "Density plot of Residuals",
          xlab = "Residuals - SRL")
shapiro.test(Ares$logResS) #almost normal

ggqqplot(Ares$StResS)
ggdensity(Ares$StResS,
          main = "Density plot of Residuals",
          xlab = "Residuals - SRL")
shapiro.test(Ares$StResS) #almost normal

ggqqplot(Ares$StNetResS)
ggdensity(Ares$StNetResS,
          main = "Density plot of Residuals",
          xlab = "Residuals - SRL")
shapiro.test(Ares$StNetResS) #not normal

#Split by groups
library(rstatix)

dfAR %>%
  ggplot(aes(x=Residuals_SRL, fill=Publication))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Residuals - SRL")

dfAR %>%
  group_by(Publication) %>%
  shapiro_test(Residuals_SRL) %>%
  ungroup()

dfAR %>%
  ggplot(aes(x=NetRes_S, fill=Publication))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Residuals - SRL")

dfAR %>%
  group_by(Publication) %>%
  shapiro_test(NetRes_S) %>%
  ungroup()

dfAR %>%
  ggplot(aes(x=logResS, fill=Publication))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Residuals - SRL")

dfAR %>%
  group_by(Publication) %>%
  shapiro_test(logResS) %>%
  ungroup()

dfAR %>%
  ggplot(aes(x=StResS, fill=Publication))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Residuals - SRL")

dfAR %>%
  group_by(Publication) %>%
  shapiro_test(StResS) %>%
  ungroup()

dfAR %>%
  ggplot(aes(x=Residuals_SRL, fill=Age_Class))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Residuals - SRL")

dfAR %>%
  group_by(Age_Class) %>%
  shapiro_test(Residuals_SRL) %>%
  ungroup()

dfAR %>%
  ggplot(aes(x=NetRes_S, fill=Age_Class))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Residuals - SRL")

dfAR %>%
  group_by(Age_Class) %>%
  shapiro_test(NetRes_S) %>%
  ungroup()

 dfAR %>%
  ggplot(aes(x=SSq_SRL, fill=Age_Class))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Residuals - SRL")

dfAR %>%
  group_by(Age_Class) %>%
  shapiro_test(SSq_SRL) %>%
  ungroup()

dfAR %>%
  ggplot(aes(x=logResS, fill=Age_Class))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Residuals - SRL")

dfAR %>%
  group_by(Age_Class) %>%
  shapiro_test(logResS) %>%
  ungroup()

dfAR %>%
  ggplot(aes(x=StResS, fill=Age_Class))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Residuals - SRL")

dfAR %>%
  group_by(Age_Class) %>%
  shapiro_test(StResS) %>%
  ungroup()


###Variance----
####Ratios----
AC$Age_Class <- as.factor(AC$Age_Class)
leveneTest(LJR_SRL ~ Age_Class, data = AC) #equal var
leveneTest(SRL_TL ~ Age_Class, data = AC) #equal var
leveneTest(SRL_TRL ~ Age_Class, data = AC) #equal var
leveneTest(TL_curve ~ Age_Class, data = AC) #equal var

####Residuals----
Ares$Publication <- as.factor(Ares$Publication)
Ares$Age_Class <- as.factor(Ares$Age_Class)
leveneTest(TLCalc_SRL ~ Publication*Age_Class, data = Ares) #equal var
leveneTest(TLCalc_SRL ~ Publication, data = Ares) #equal var
leveneTest(TLCalc_SRL ~ Age_Class, data = Ares) #equal var

leveneTest(TLCalc_TRL ~ Publication*Age_Class, data = Ares) #equal var
leveneTest(TLCalc_TRL ~ Publication, data = Ares) #equal var
leveneTest(TLCalc_TRL ~ Age_Class, data = Ares) #equal var

leveneTest(Residuals_SRL ~ Publication*Age_Class, data = Ares) #equal var
leveneTest(Residuals_SRL ~ Publication, data = Ares) #equal var
leveneTest(Residuals_SRL ~ Age_Class, data = Ares) #equal var

##Test Ratios----
###Ratio Reliability----
#NOT NORMAL
#Variance Equal 

aovA0 <- aov(LJR_SRL ~ State, data = AC) #1Way
summary(aovA0)

wilcox.test(formula = LJR_SRL~ Rost_Real, data = dfA)
wilcox.test(formula = LJR_SRL~ Multiple_Planes, data = dfA)
wilcox.test(formula = LJR_SRL~ TL_Real, data = dfA)

aovA1 <- aov(SRL_TRL ~ State, data = AC) #1Way
summary(aovA1)

t.test(formula = SRL_TRL~ Rost_Real, data = dfA)
t.test(formula = SRL_TRL~ Multiple_Planes, data = dfA)
t.test(formula = SRL_TRL~ TL_Real, data = dfA)

aovA2 <- aov(SRL_TL ~ State, data = AC) #1Way
summary(aovA2)

t.test(formula = SRL_TL~ Rost_Real, data = dfA)
t.test(formula = SRL_TL~ Multiple_Planes, data = dfA)
t.test(formula = SRL_TL~ TL_Real, data = dfA)

###Ratio by Age----

aovA3 <- aov(SRL_TRL ~ Age_Class, data = AC) #1Way
summary(aovA3)
aovA4 <- aov(SRL_TL ~ Age_Class, data = AC) #1Way
summary(aovA4)
aovA5 <- aov(TRL_TL ~ Age_Class, data = AC) #1Way
summary(aovA5)

kruskal.test(LJR_SRL ~ Age_Class, data = AC)

wilcox.test(formula = LJR_SRL~ Age2, data = dfA)
wilcox.test(formula = LJR_SRL~ Age3, data = dfA)
wilcox.test(formula = LJR_SRL~ Age4, data = dfA)
wilcox.test(formula = LJR_SRL~ Age5, data = dfA)

t.test(formula = SRL_TL ~ Age2, data = dfA)
t.test(formula = SRL_TL ~ Age3, data = dfA)
t.test(formula = SRL_TL ~ Age4, data = dfA)
t.test(formula = SRL_TL ~ Age5, data = dfA)

t.test(formula = SRL_TRL ~ Age2, data = dfA)
t.test(formula = SRL_TRL ~ Age3, data = dfA)
t.test(formula = SRL_TRL ~ Age4, data = dfA)
t.test(formula = SRL_TRL ~ Age5, data = dfA)

AC_SRL <- AC$SRL 
AC_TL <- AC$TL_curve
cor(AC_SRL, AC_TL, use = "pairwise.complete.obs")
ggplot(AC, aes(x=SRL, y=TL_curve, color=Age2)) + 
  geom_point()+
  geom_smooth(method=lm)

ggplot(AC, aes(x=Age_Class, y=LJR_SRL, color=Age_Class)) + 
  geom_boxplot() +
  #coord_flip() +
  labs(title = "LJR:SRL Anoxy")

ggplot(AC, aes(x=Age2, y=LJR_SRL, color=Age2)) + 
  geom_boxplot() +
  #coord_flip() +
  labs(title = "LJR:SRL Anoxy")

ggplot(AC, aes(x=Age_Class, y=SRL_TL, color=Age_Class)) + 
  geom_boxplot() +
  #coord_flip() +
  labs(title = "SRL_TL Anoxy")

ggplot(AC, aes(x=Age2, y=SRL_TL, color=Age2)) + 
  geom_boxplot() +
  #coord_flip() +
  labs(title = "SRL_TL Anoxy")

ggplot(AC, aes(x=Age_Class, y=SRL_TRL, color=Age_Class)) + 
  geom_boxplot() +
  #coord_flip() +
  labs(title = "SRL_TRL Anoxy")

ggplot(AC, aes(x=Age2, y=SRL_TRL, color=Age2)) + 
  geom_boxplot() +
  #coord_flip() +
  labs(title = "SRL_TRL Anoxy")

##TRL vs SRL----
dfAR %>%  #ttest - calc vs true SRL
  do(tidy(wilcox.test(.$NetRes_S,
                      .$NetRes_T,
                      mu = 0,
                      alt = "greater",
                      paired = TRUE,
                      conf.level = 0.99)))

dfAR %>%  #ttest - calc vs true SRL
  do(tidy(t.test(.$StNetResS,
                 .$StNetResT,
                 mu = 0,
                 alt = "two.sided",
                 paired = TRUE,
                 conf.level = 0.99)))

dfAR %>%  #ttest - calc vs true SRL
  do(tidy(t.test(.$logResS,
                 .$logResT,
                 mu = 0,
                 alt = "two.sided",
                 paired = TRUE,
                 conf.level = 0.99)))

dfAR %>%  #ttest - calc vs true SRL
  group_by(Maturity) %>%
  do(tidy(wilcox.test(.$NetRes_S,
                      .$NetRes_T,
                      mu = 0,
                      alt = "greater",
                      paired = TRUE,
                      exact = FALSE,
                      conf.level = 0.99))) %>%
  ungroup()

dfAR %>%  #ttest - calc vs true SRL
  group_by(Publication) %>%
  do(tidy(wilcox.test(.$NetRes_S,
                      .$NetRes_T,
                      mu = 0,
                      alt = "greater",
                      paired = TRUE,
                      exact = FALSE,
                      conf.level = 0.99))) %>%
  ungroup()

dfAR %>%  #ttest - calc vs true SRL
  group_by(Publication) %>%
  do(tidy(t.test(.$logResS,
                 .$logResT,
                 mu = 0,
                 alt = "two.sided",
                 paired = TRUE,
                 exact = FALSE,
                 conf.level = 0.99))) %>%
  ungroup()

dfAR %>%  #ttest - calc vs true SRL
  group_by(Publication, Maturity) %>%
  do(tidy(t.test(.$StNetResS,
                 .$StNetResT,
                 mu = 0,
                 alt = "greater",
                 paired = TRUE,
                 exact = FALSE,
                 conf.level = 0.99))) %>%
  ungroup()

##Isometry AC----

model_A <- lm(log(SRL) ~ log(TRL), data = AC)
summary(model_A)

model_A1 <- lm(log(SRL) ~ log(TL_curve), data = AC)
summary(model_A1)

model_A2 <- lm(log(TRL) ~ log(TL_curve), data = AC)
summary(model_A2)

####TRL vs SRL Bar----

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- plyr::rename(data_sum, c("mean" = varname))
  return(data_sum)
}

ASNRP <- data_summary(Ares, varname = "StNetResS", 
                      groupnames=c("Publication", "Maturity", "ModelT"))

ASNRP$Maturity=as.factor(ASNRP$Maturity)
head(ASNRP)

library(ggpattern)

StBarAR <- ggplot(ASNRP[order(ASNRP$Maturity,decreasing=T),],
                  aes(x=reorder(Publication, +StNetResS),
                      y=StNetResS, 
                      fill=factor(Maturity, levels = c("Mature", "Immature")),
                      pattern = ModelT)) + 
  geom_bar(stat="identity", 
           position="stack") +
  geom_bar_pattern(stat="identity",
                   position = "stack",
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.09,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(values = c(Lin = "stripe", Pow = "none", Quad = "circle"),
                       labels = c('Linear', 'Power', 'Quadratic')) +
  geom_text(aes(label = paste(round(StNetResS,1),"±", round(sd,1))),
            position = position_stack(), hjust = 1.2, size = 3.5, fontface = "bold", color = "white") +
  labs(title = "SRL Input", x = "Publication", y = "Standardised Net Residuals Produced", pattern = "Model Type") +
  theme_minimal() +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))+
  scale_x_discrete(labels = c('Whitty et al. 2014',
                              'Faria et al. 2007',
                              'Peverell, 2010',
                              'Faria et al. 2007',
                              'Whitty et al. 2014'
                              )) +
  theme(legend.position = c(0.8, 0.3), 
        legend.background = element_rect(fill="white")) +
  coord_flip() +
  ylim(0, 52) +
  scale_fill_manual(name = "Maturity",
                    labels=c('Mature', 'Immature'), 
                    values = c("grey40", "grey70"))
StBarAR

ASNRT <- data_summary(Ares, varname = "StNetResT", 
                      groupnames=c("Publication", "Maturity", "ModelT"))

ASNRT$Maturity=as.factor(ASNRT$Maturity)
head(ASNRT)

StBarART <- ggplot(ASNRT[order(ASNRT$Maturity,decreasing=T),],
                   aes(x=reorder(Publication, +StNetResT),
                       y=StNetResT, 
                       fill=factor(Maturity, levels = c("Mature", "Immature")),
                       pattern = ModelT)) + 
  geom_bar(stat="identity", 
           position="stack") +
  geom_bar_pattern(stat="identity",
                   position = "stack",
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.09,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(values = c(Lin = "stripe", Pow = "none", Quad = "circle"),
                       labels = c('Linear', 'Power', 'Quadratic')) +
  geom_text(aes(label = paste(round(StNetResT,1),"±", round(sd,1))),
            position = position_stack(), hjust = 1.2, size = 3.5, fontface = "bold", color = "white") +
  labs(title = "TRL Input", x = "Publication", y = "Standardised Net Residuals Produced", pattern = "Model Type") +
  theme_minimal() +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))+
  scale_x_discrete(labels = c('Whitty et al. 2014',
                              'Faria et al. 2007',
                              'Faria et al. 2007',
                              'Peverell, 2010',
                              'Whitty et al. 2014'
  )) +
  theme(legend.position = c(0.9, 0.6), legend.background = element_rect(fill="white")) +
  coord_flip() +
  ylim(0, 52) +
  scale_fill_manual(name = "Maturity",
                    labels=c('Mature', 'Immature'), 
                    values = c("grey40", "grey70"))
StBarART

#TRLvsSRL - Acuspidata discussion
cowplot::plot_grid(StBarAR + theme(axis.text.x = element_blank(),
                                   axis.title.x = element_blank(),
                                   legend.position = "none"),
                   StBarART,
                   labels = "AUTO", ncol = 1, align ="v",
                   rel_heights = c(.85,1))
ggsave(
  "AnoxyTRL.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 10,
  height = 6,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

##Residuals by Age----
library("dplyr")
sumtable(Ares, group = "Age_Class", group.long=T) #as Table

###t-test Age----

wtANET <- Ares %>% #wilcoxtest
  group_by(Age_Class) %>%
  do(tidy(wilcox.test(.$NetRes_S,
                      .$NetRes_T,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      exact = FALSE,
                      conf.level = 0.99))) %>%
  ungroup()
wtANET

Ares %>%
  group_by(Age_Class) %>% #means Residuals by group Age Class
  dplyr::summarise(
    count = n(),
    mean = mean(SSq_SRL, na.rm = TRUE),
    sd = sd(SSq_SRL, na.rm = TRUE)) %>%
  ungroup()

Ares %>%
  group_by(Age_Class) %>% #means Residuals by group Age Class
  dplyr::summarise(
    count = n(),
    mean = mean(SSq_TRL, na.rm = TRUE),
    sd = sd(SSq_TRL, na.rm = TRUE)) %>%
  ungroup()

Ares %>%
  group_by(Age_Class) %>% #means Residuals by group Age Class
  dplyr::summarise(
    count = n(),
    mean = mean(NetRes_S, na.rm = TRUE),
    sd = sd(NetRes_S, na.rm = TRUE)) %>%
  ungroup()

Ares %>%
  group_by(Age_Class) %>% #means Residuals by group Age Class
  dplyr::summarise(
    count = n(),
    mean = mean(Residuals_SRL, na.rm = TRUE),
    sd = sd(Residuals_SRL, na.rm = TRUE)) %>%
  ungroup()

Ares %>%
  group_by(Age_Class) %>% #means Residuals by group Age Class
  dplyr::summarise(
    count = n(),
    mean = mean(Residuals_TRL, na.rm = TRUE),
    sd = sd(Residuals_TRL, na.rm = TRUE)) %>%
  ungroup()

Ares %>%
  group_by(Age_Class) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)) %>%
  ungroup()

Ares %>%
  group_by(Age_Class) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResT, na.rm = TRUE),
    sd = sd(StNetResT, na.rm = TRUE)) %>%
  ungroup()

wtASRLAge <- Ares %>% #wilcoxtest
  group_by(Age_Class) %>%
  do(tidy(wilcox.test(.$TLCalc_SRL,
                      .$TLTrue,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      exact = FALSE,
                      correct = TRUE,
                      conf.level = 0.99, 
                      na.action = na.omit))) %>%
  ungroup()
wtASRLAge

wtASRLAge2 <- Ares %>% #wilcoxtest
  group_by(Maturity) %>%
  do(tidy(wilcox.test(.$TLCalc_SRL,
                      .$TLTrue,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      exact = FALSE,
                      correct = TRUE,
                      conf.level = 0.99, 
                      na.action = na.omit))) %>%
  ungroup()
wtASRLAge2

Ares %>% #wilcoxtest
  group_by(Age_Class) %>%
  do(tidy(wilcox.test(.$Residuals_SRL,
                      .$Residuals_TRL,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      exact = FALSE,
                      correct = TRUE,
                      conf.level = 0.99, 
                      na.action = na.omit))) %>%
  ungroup()

###ANOVA/Krusk Age----
aovAAge <- aov(Residuals_SRL ~ Age_Class, Ares)
summary(aovAAge)

kruskal.test(Residuals_SRL ~ Age_Class, data = Ares)
kruskal.test(NetRes_S ~ Age_Class, data = Ares)
kruskal.test(logResS ~ Age_Class, data = Ares)

kruskal.test(Residuals_TRL ~ Age_Class, data = Ares)
kruskal.test(NetRes_T ~ Age_Class, data = Ares)
kruskal.test(logResT ~ Age_Class, data = Ares)

###Boxplot----
ggplot(Ares, aes(Age_Class, Residuals_SRL, color=Age_Class)) +
  geom_boxplot() +
  #coord_flip() +
  labs(title = "Projected Total Length of Sawfishes ", 
       x= bquote(NULL),
       y= bquote("Residuals"))

##Residuals by Pub----
sumtable(Ares, group = "Publication", group.long=T) #as Table

###Means Pub----
Ares %>%
  group_by(Publication) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(Residuals_SRL, na.rm = TRUE),
    sd = sd(Residuals_SRL, na.rm = TRUE)) %>%
  ungroup()

Ares %>%
  group_by(Publication) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(Residuals_TRL, na.rm = TRUE),
    sd = sd(Residuals_TRL, na.rm = TRUE)) %>%
  ungroup()

Ares %>%
  group_by(Publication) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(SSq_SRL, na.rm = TRUE),
    sd = sd(SSq_SRL, na.rm = TRUE)) %>%
  ungroup()

Ares %>%
  group_by(Publication) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(NetRes_S, na.rm = TRUE),
    sd = sd(NetRes_S, na.rm = TRUE)) %>%
  ungroup()

Ares %>%
  group_by(Publication) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(NetRes_T, na.rm = TRUE),
    sd = sd(NetRes_T, na.rm = TRUE)) %>%
  ungroup()

Ares %>%
  group_by(Publication) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(SSq_TRL, na.rm = TRUE),
    sd = sd(SSq_TRL, na.rm = TRUE)) %>%
  ungroup()

Ares %>%
  group_by(Publication) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(StResS, na.rm = TRUE),
    sd = sd(StResS, na.rm = TRUE)) %>%
  ungroup()

Ares %>%
  group_by(Publication) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(StResT, na.rm = TRUE),
    sd = sd(StResT, na.rm = TRUE)) %>%
  ungroup()

Ares %>%
  group_by(Publication) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)) %>%
  ungroup()

Ares %>%
  group_by(Publication) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResT, na.rm = TRUE),
    sd = sd(StNetResT, na.rm = TRUE)) %>%
  ungroup()

###t-tests----
library(tidyverse)
library(dplyr)
library(tibble)
library(broom)

pwASRL <- Ares %>%  #ttest - calc vs true SRL
  group_by(Publication) %>%
  do(tidy(t.test(.$TLCalc_SRL,
                 .$TLTrue,
                 mu = 0,
                 alt = "two.sided",
                 paired = TRUE,
                 conf.level = 0.99))) %>%
  ungroup()
pwASRL

pwATRL <- Ares %>%  #ttest - calc vs true TRL
  group_by(Publication) %>%
  do(tidy(t.test(.$TLCalc_TRL,
                 .$TLTrue,
                 mu = 0,
                 alt = "two.sided",
                 paired = TRUE,
                 conf.level = 0.99))) %>%
  ungroup()
pwATRL

AStd <- Ares %>%  #test - Standardised Raw
  group_by(Publication) %>%
  do(tidy(t.test(.$StResS,
                 .$StResT,
                 mu = 0,
                 alt = "two.sided",
                 paired = TRUE,
                 conf.level = 0.99))) %>%
  ungroup()
AStd

wStd <- Ares %>%  # wilcox test - Standardised Raw
  group_by(Publication) %>%
  do(tidy(wilcox.test(.$StResS,
                 .$StResT,
                 mu = 0,
                 alt = "two.sided",
                 paired = TRUE,
                 exact = FALSE,
                 conf.level = 0.99))) %>%
  ungroup()
wStd

wtASRL <- Ares %>% #wilcoxtest
  group_by(Publication) %>%
  do(tidy(wilcox.test(.$TLCalc_SRL,
                      .$TLTrue,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      exact = FALSE,
                      conf.level = 0.99))) %>%
  ungroup()
wtASRL

wtASSq <- Ares %>% #wilcoxtest
  group_by(Publication) %>%
  do(tidy(wilcox.test(.$SSq_SRL,
                      .$SSq_TRL,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      exact = FALSE,
                      conf.level = 0.99))) %>%
  ungroup()
wtASSq

wtANET <- Ares %>% #wilcoxtest
  group_by(Publication) %>%
  do(tidy(wilcox.test(.$logResS,
                      .$logResT,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      exact = FALSE,
                      na.omit = TRUE,
                      conf.level = 0.99))) %>%
  ungroup()
wtANET

wilcox.test(Atest$LogP, Atest$LogFL,
                      alt = "two.sided",
                      paired = TRUE,
                      conf.level = 0.99)

wilcox.test(Atest$LogP, Atest$LogWL,
            mu = 0,
            alt = "two.sided",
            paired = TRUE,
            conf.level = 0.99)

wilcox.test(Atest$LogWL, Atest$LogFL,
            mu = 0,
            alt = "two.sided",
            paired = TRUE,
            conf.level = 0.99)

###ANOVA Pub----
aovAPUB <- aov(logResS ~ Publication, Ares)
summary(aovAPUB)

kruskal.test(TLCalc_SRL ~ Publication, Ares)
kruskal.test(Residuals_SRL ~ Publication, Ares)
kruskal.test(SSq_SRL ~ Publication, Ares)
kruskal.test(NetRes_S ~ Publication, Ares)

###Resid Corr----
sapply(
  split(data.frame(Ares$TLTrue, Ares$TLCalc_SRL), Ares$Publication), 
  function(x) cor(x[[1]],x[[2]])
)

##Association Age-Pub----

###Means----
Ares %>%
  group_by(Publication, Maturity) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)) %>%
  ungroup()

Ares %>%
  group_by(Publication, Maturity) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(NetRes_S, na.rm = TRUE),
    sd = sd(NetRes_S, na.rm = TRUE)) %>%
  ungroup()

Ares %>%
  group_by(Publication, Age_Class) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)) %>%
  ungroup()

Ares %>%
  group_by(Publication, Age_Class) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(NetRes_S, na.rm = TRUE),
    sd = sd(NetRes_S, na.rm = TRUE)) %>%
  ungroup()

Ares %>%
  group_by(Publication, Maturity) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResT, na.rm = TRUE),
    sd = sd(StNetResT, na.rm = TRUE)) %>%
  ungroup()

Ares %>%
  group_by(Publication, Age_Class) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(NetRes_T, na.rm = TRUE),
    sd = sd(NetRes_T, na.rm = TRUE)) %>%
  ungroup()

dfAMinus <- Ares[Ares$Publication!= 'Whitty Quad', ]

###T-test Split by Pubs----
wilcox.test(formula = NetP ~ Maturity, data = dfAT)
wilcox.test(formula = NetFL ~ Maturity, data = dfAT)
wilcox.test(formula = NetFP ~ Maturity, data = dfAT)
wilcox.test(formula = NetWQ ~ Maturity, data = dfAT)
wilcox.test(formula = NetWL ~ Maturity, exact = FALSE, data = dfAT)
wilcox.test(formula = NetBL ~ Maturity, data = dfAT)
wilcox.test(formula = NetBE ~ Maturity, data = dfAT)
wilcox.test(formula = NetBQ ~ Maturity, data = dfAT)

t.test(formula = LogP ~ Maturity, data = dfAT)
t.test(formula = LogFL ~ Maturity, data = dfAT)
t.test(formula = LogFP ~ Maturity, data = dfAT)
t.test(formula = LogWQ ~ Maturity, data = dfAT)
t.test(formula = LogWL ~ Maturity, exact = FALSE, data = dfAT)

wilcox.test(formula = SNStP ~ Maturity, data = dfAT)
wilcox.test(formula = SNStFL ~ Maturity, data = dfAT)
wilcox.test(formula = SNStFP ~ Maturity, data = dfAT)
wilcox.test(formula = SNStWQ ~ Maturity, data = dfAT)
wilcox.test(formula = SNStWL ~ Maturity, data = dfAT)

t.test(formula = SNStP ~ Maturity, data = dfAT)
t.test(formula = SNStFL ~ Maturity, data = dfAT)
t.test(formula = SNStFP ~ Maturity, data = dfAT)
t.test(formula = SNStWQ ~ Maturity, data = dfAT)
t.test(formula = SNStWL ~ Maturity, data = dfAT)

###ANOVA Split by Pubs----

kruskal.test(NetP ~ Age_Class, data = Atest) #1Way
kruskal.test(NetFL ~ Age_Class, data = Atest) #1Way
kruskal.test(NetFP ~ Age_Class, data = Atest) #1Way
kruskal.test(NetWQ ~ Age_Class, data = Atest) #1Way
kruskal.test(NetWL ~ Age_Class, data = Atest) #1Way
kruskal.test(NetBL ~ Age_Class, data = Atest) #1Way
kruskal.test(NetBE ~ Age_Class, data = Atest) #1Way

kruskal.test(SNStP ~ Age_Class, data = Atest) #1Way
kruskal.test(SNStFL ~ Age_Class, data = Atest) #1Way
kruskal.test(SNStFP ~ Age_Class, data = Atest) #1Way
kruskal.test(SNStWL ~ Age_Class, data = Atest) #1Way
kruskal.test(SNStWQ ~ Age_Class, data = Atest) #1Way

aovA2PUB <- aov(Residuals_SRL ~ Publication + Age_Class, Ares)
summary(aovA2PUB)

aovA21PUB <- aov(Residuals_SRL ~ Publication + Age_Class, dfAMinus)
summary(aovA21PUB)

aovA2PUB <- aov(NetRes_S ~ Publication + Age_Class + Publication:Age_Class, Ares)
summary(aovA2PUB)

aovA21PUB <- aov(NetRes_S ~ Publication + Age_Class, dfAMinus)
summary(aovA21PUB)

aovA2PUB <- aov(StResS ~ Publication + Age_Class, Ares)
summary(aovA2PUB)

aovA21PUB <- aov(StResS ~ Publication + Age_Class, dfAMinus)
summary(aovA21PUB)

aovAM <- aov(StNetResS ~ ModelT + Maturity + ModelT:Maturity, data = Ares)
summary(aovAM)

interaction.plot(x.factor     = Ares$ModelT,
                 trace.factor = Ares$Maturity,
                 response     = Ares$StNetResS,
                 fun = mean,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")

Ares %>% #wilcox test
  group_by(Publication, Maturity) %>%
  do(tidy(wilcox.test(.$TLCalc_SRL,
                      .$TLTrue,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      conf.level = 0.99))) %>%
  ungroup()

Ares %>% #wilcox test
  group_by(Publication, Maturity) %>%
  do(tidy(wilcox.test(.$Residuals_SRL,
                      .$Residuals_TRL,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      conf.level = 0.99))) %>%
  ungroup()

Ares %>% #t test
  group_by(Publication, Maturity) %>%
  do(tidy(t.test(.$StResS,
                      .$StResT,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      conf.level = 0.99))) %>%
  ungroup()

Ares %>% #wilcox test
  group_by(Publication, Age_Class) %>%
  do(tidy(wilcox.test(.$StResS,
                 .$StResT,
                 mu = 0,
                 alt = "greater",
                 paired = TRUE,
                 conf.level = 0.99))) %>%
  ungroup()

###Boxplot Pub-Age----
ggplot(Ares, aes(Age_Class, Residuals_SRL, color=Publication)) +
  geom_boxplot() +
  #coord_flip() +
  labs(title = "Estimated Total Length - A. cuspidata ", 
       x= bquote(NULL),
       y= bquote("Residuals"))

ggplot(Ares, aes(Age_Class, Residuals_TRL, color=Publication)) +
  geom_boxplot() +
  #coord_flip() +
  labs(title = "Projected Total Length of Anoxy TRL ", 
       x= bquote(NULL),
       y= bquote("Residuals"))

##Best Fit----

model_A3 <- lm(TL_curve ~ SRL, data = AC)

model_A4 <- lm(TL_curve ~ TRL, data = AC)

model_A5 <- lm(TL_curve ~ SRL + TRL, data = AC)

model_A6 <- lm(TL_curve ~ SRL + SRL_TRL, data = AC)

library(AICcmodavg)

modelsA <- list(model_A3, model_A4, model_A5, model_A6)
modAnam <- c('SRL', 'TRL', 'both', 'SRL plus rat')

aictab(cand.set = modelsA,
       modnames = modAnam)

modAP <- lm(SStP ~ TLCorr, data = Atest)
modAFL <- lm(SStFL ~ TLCorr, data = Atest)
modAFP <- lm(SStFP~ TLCorr, data = Atest)
modAWL <- lm(SStWL~ TLCorr, data = Atest)
modAWQ<- lm(SStWQ~ TLCorr, data = Atest)

modelsAPub <- list(modAPev, modAFL, modAFP, modAWL, modAWQ)
modAnamP <- c('P', 'FL', 'FP', 'WL', 'WQ')

summary(modAP)
summary(modAFL)
summary(modAFP)
summary(modAWL)
summary(modAWQ)

aictab(cand.set = modelsAPub,
       modnames = modAnamP)

##ToPrint----
###BarPlot Pub-Age----

#NewDF

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
 
  ARP <- data_summary(Ares, varname = "Residuals_SRL", 
                      groupnames=c("Publication", "Age_Class"))

library(scales)
  
ARP$Age_Class=as.factor(ARP$Age_Class)
head(ARP)

BarAR <- ggplot(ARP, aes(x=factor(Publication, level = c("Whitty Linear",
                                                         "Faria Linear",
                                                         "Faria Power",
                                                         "Peverell",
                                                         "Whitty Quad")),
                         y=Residuals_SRL, 
                         fill=factor(Age_Class, level = c('YOY',
                                                          'juvenile',
                                                          'adult')))) + 
  geom_bar(stat="identity", 
           color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Residuals_SRL-sd, 
                    ymax=Residuals_SRL+sd), 
                width=.2,
                position=position_dodge(.9)) +
  labs(title = "A. cuspidata", x = "Model", y = "Residuals (mm)") +
  theme(plot.title = element_text(face = "italic", 
                                  size = 14),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  scale_x_discrete(labels = c("WR", "FL", "FP", "P", "W")) +
  scale_fill_manual(name = "Size Class",
                    labels=c('YOY', 'Juvenile', 'Adult'), 
                    values = c("#5ab4ac", "#f5f5f5", "#d8b365")) 

ACwrap <- c("Whitty et al. (2014)  Ratio",
  "Faria (2007) Linear",
  "Faria (2007) Power",
  "Peverell (2010)",
  "Whitty et al. (2014)  Quadratic")

wrap_labelsA <- function(x) {
  str_wrap(ACwrap, width = 16)
}

print(BarAR)
# Finished bar plot

###Stacked Bar----

ANRP <- data_summary(Ares, varname = "NetRes_S", 
                    groupnames=c("Publication", "Maturity"))

ANRP$Maturity=as.factor(ANRP$Maturity)
head(ANRP)

SNBarAR <- ggplot(ANRP, aes(x=NetRes_S,
                          y=Publication, 
                          fill=Maturity)) + 
  geom_bar(stat="identity", 
           position="stack") +
  labs(title = "A. cuspidata", x = "Publication", y = "Net Residuals") +
  theme(plot.title = element_text(face = "italic")) +
  theme_minimal() +
  scale_fill_manual(name = "Maturity",
                    labels=c('Immature', 'Mature'), 
                    values = c("grey90", "grey40"))
SNBarAR


###DotPlot----
ACDP <- ggplot(Ares, mapping=aes(x=TLTrue, 
                                 y=StResS, 
                                 color=factor(Publication,
                                              levels = c('Whitty Linear',
                                                         'Whitty Quad', 
                                                         'Faria Linear',
                                                         'Peverell',
                                                         'Faria Power')))) +
  coord_cartesian(xlim =c(550, 4100)) +
  geom_point(size = 0.75) +
  geom_hline(yintercept = c(-5, 0, 5),
             linetype = c('dotted', "longdash", 'dotted'),
             color = c('grey40', 'black', 'grey40')) +
  annotate("rect", xmin = -620, xmax = 4320, 
           ymin = -5.2, ymax= 5.2, alpha = 0.1, fill = 'grey40') +
  scale_color_manual(labels=c('Whitty et al. (2014) Ratio',
                              'Whitty et al. (2014) Quadratic',
                              'Faria (2007) Linear',
                              'Peverell (2010)', 
                              'Faria (2007) Power'),
                     values = c('darkblue', 
                                'lightblue',
                                'orange', 
                                rgb(0.2,0.4,0.1,0.7), 
                                'yellow')) +
  stat_smooth(aes(x=TLTrue, y=StResS, color=Publication), 
              method = lm, se = FALSE) +
  geom_vline(xintercept = c(560, 1385, 1820, 2200, 4090),
             linetype = c('solid', 'solid', 'dotdash', 'solid', 'solid')) +
  # annotate(geom = "text",
  #          label = c('Birth Size', 'One Year', ' ', 'Maturity', 'Max Size'),
  #          x = c(560, 1385, 1820, 2200, 4090),
  #          y = c(50, 50, 50, 50, 50),
  #          size = c(4,4,4,4,4),
  #          angle = 90,
  #          vjust = -1) +
  geom_segment(x = 560, y = 62, xend = 1385, yend = 62,
               colour = "#5ab4ac",
               size = 3) +
  geom_segment(x = 1385, y = 62, xend = 1820, yend = 62,
               colour = "#f5f5f5",
               size = 3) +
  geom_segment(x = 1820, y = 62, xend = 2200, yend = 62,
               colour = "lightgrey",
               size = 3) +
  geom_segment(x = 2200, y = 62, xend = 4090, yend = 62,
               colour = "#d8b365",
               size = 3) +
  # annotate(geom = "label",
  #          label = c('YOY', 'Juvenile', 'Sub-Adult', 'Adult'),
  #          x = c((560+1385)/2, (1385+1820)/2, (1820+2200)/2, (2200+4090)/2),
  #          y = c(75, 75, 75, 75),
  #          fill = c("#5ab4ac", "#f5f5f5", "lightgrey", "#d8b365"),
  #          colour = c("white", "black", "black", "white"),
  #          size = c(4,4,4,4),
  #          angle = 0,
  #          vjust = 1) +
  theme(plot.title = element_text(size = 14, face = "italic"),
        legend.key = element_rect(fill = "white"),
        legend.position = c(0.825, .3),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  labs(title = "Anoxypristis cuspidata",
       x = bquote("Total Length (mm)"),
       y = bquote("Percent Error (%)"),
       color = "Model")

ACDP

ggplot(Ares, mapping=aes(x=TLTrue, y=StResT, color=Publication)) +
  geom_point() +
  geom_hline(yintercept=0,linetype="dashed") +
  stat_ellipse(aes(x=TLTrue, y=StResT, color=Publication),type = "norm") +
  scale_fill_manual(values = c("blue4",
                               "gold2", 
                               "red3", 
                               "yellow", 
                               "darkgreen",
                               "grey", 
                               "lightgrey")) +
  labs(title = "Anoxypristis Total Length SRL",
       x = bquote("Total Length"),
       y = bquote("Residuals"),
       fill = "Publication")

###Pub Section Pic----
#Dot + Box

cowplot::plot_grid(BarAR + theme(plot.title = element_blank(),
                                 legend.position = "none"),
                   ACDP + theme(plot.title = element_blank()),
                   labels = "auto", ncol = 1, rel_heights = c(1,2), align ="v")

ggsave(
  "AnoxyData.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 10,
  height = 6,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

##Appdx: Residual Plot----
###Big Boi----
library(cowplot)
Aa <- ggplot(dfAT, aes(x = SRL, y = TLCorr)) + #Pev
  geom_smooth(aes(x = SRL, y = TL_Pev), method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = SRL, yend = TL_Pev), alpha = .2) +
  ylim(0, 3800) +
  geom_point(aes(color = ResidP)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  geom_point(aes(y = TLCorr), shape = 1) +
  labs(x= "SRL", y = "Total Length", 
       title= "Peverell, 2010", color ="Residuals") +
  theme_bw()
Ab <- ggplot(dfAT, aes(x = SRL, y = TLCorr)) + #FariaL
  geom_smooth(aes(x = SRL, y = TL_FarL), method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = SRL, yend = TL_FarL), alpha = .2) +
  ylim(0, 3500) +
  geom_point(aes(color = ResidFL)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  geom_point(aes(y = TLCorr), shape = 1) +
  labs(x= "SRL", y = "Total Length", 
       title= "Faria, 2007 - Linear", color ="Residuals") +
  theme_bw()
Ac <- ggplot(dfAT, aes(x = SRL, y = TLCorr)) + #FariaE
  stat_smooth(aes(x = SRL, y = TL_FarE), method = "loess", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = SRL, yend = TL_FarE), alpha = .2) +
  ylim(0, 3500) +
  geom_point(aes(color = ResidFE)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  geom_point(aes(y = TLCorr), shape = 1) +
  labs(x= "SRL", y = "Total Length", 
       title= "Faria, 2007 - Power", color ="Residuals") +
  theme_bw()
Ad <- ggplot(dfAT, aes(x = SRL, y = TLCorr)) + #WhittyQ
  stat_smooth(aes(x = SRL, y = TL_WQ), method = "loess", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = SRL, yend = TL_WQ), alpha = .2) +
  ylim(0, 3500) +
  geom_point(aes(color = ResidWQ)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  geom_point(aes(y = TLCorr), shape = 1) +
  labs(x= "SRL", y = "Total Length", 
       title= "Whitty et al. 2014 - Quadratic", color ="Residuals") +
  theme_bw()
Ae <- ggplot(dfAT, aes(x = SRL, y = TLCorr)) + #WhittyL
  geom_smooth(aes(x = SRL, y = TL_WL), method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = SRL, yend = TL_WL), alpha = .2) +
  ylim(0, 3500) +
  geom_point(aes(color = ResidWL)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  geom_point(aes(y = TLCorr), shape = 1) +
  labs(x= "SRL", y = "Total Length", 
       title= "Whitty et al. 2014 - Ratio", color ="Residuals") +
  theme_bw()
Af <- ggplot(dfAT, aes(x = SRL, y = TLCorr)) + #BiskisL
  geom_smooth(aes(x = SRL, y = TL_BL), method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = SRL, yend = TL_BL), alpha = .2) +
  geom_point(aes(color = ResidBL)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  geom_point(aes(y = TLCorr), shape = 1) +
  labs(x= "SRL", y = "Total Length", 
       title= "Current Study - Linear", color ="Residuals") +
  theme_bw()

Ag <- ggplot(dfAT, aes(x = SRL, y = TLCorr)) + #BiskisE
  geom_smooth(aes(x = SRL, y = TL_BE), method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = SRL, yend = TL_BE), alpha = .2) +
  geom_point(aes(color = ResidBE)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  geom_point(aes(y = TLCorr), shape = 1) +
  theme_bw()
Ah <- ggplot(dfAT, aes(x = SRL, y = TLCorr)) + #BiskisQ
  geom_smooth(aes(x = SRL, y = TL_BQ), method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = SRL, yend = TL_BQ), alpha = .2) +
  geom_point(aes(color = ResidBQ)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  geom_point(aes(y = TLCorr), shape = 1) +
  theme_bw()

AResGALL <- cowplot::plot_grid(Ab, 
                               Ae + theme(axis.text.y = element_blank(),
                                              axis.ticks.y = element_blank(),
                                              axis.title.y = element_blank() ),
                               Aa + theme(axis.text.y = element_blank(),
                                         axis.ticks.y = element_blank(),
                                         axis.title.y = element_blank() ),
                               Ac,
                               Ad + theme(axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank(),
                                          axis.title.y = element_blank() ),
                               # Af + theme(axis.text.y = element_blank(),
                               #            axis.ticks.y = element_blank(),
                               #            axis.title.y = element_blank() ),
                               labels = "AUTO", ncol = 3, align ="v")
AResGALL

ggsave(
  "AnoxyRes.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 10,
  height = 5,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

#Clavata----
###Normality----
####Ratios----
library(car)
library(ggpubr)
#ALL NORMAL

ggqqplot(PC$LJR_SRL)
ggdensity(PC$LJR_SRL,
          main = "Density plot of LJR/SRL",
          xlab = "LJR/SRL")
shapiro.test(PC$LJR_SRL) #normal

ggqqplot(PC$SRL_TL)
ggdensity(PC$SRL_TL,
          main = "Density plot of SRL/TL",
          xlab = "SRL_TL")
shapiro.test(PC$SRL_TL) #normal

ggqqplot(PC$SRL_TRL)
ggdensity(PC$SRL_TRL,
          main = "Density plot of SRL/TRL",
          xlab = "SRL_TL")
shapiro.test(PC$SRL_TRL) #normal

ggqqplot(PC$TL_ImgJ)
ggdensity(PC$TL_ImgJ,
          main = "Density plot of TL",
          xlab = "TL")
shapiro.test(PC$TL_ImgJ) #normal

dfC %>%
  ggplot(aes(x=LJR_SRL, fill=Age_Class))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="LJR/SRL Ratio")

dfC %>%
  ggplot(aes(x=LJR_SRL, fill=Size_Class))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="LJR/SRL Ratio")

dfC %>%
  group_by(Size_Class) %>%
  shapiro_test(LJR_SRL) %>%
  ungroup()

dfC %>%
  group_by(Age_Class) %>%
  shapiro_test(LJR_SRL) %>%
  ungroup()

dfC %>%
  ggplot(aes(x=SRL_TL, fill=Age_Class))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="SRL_TL Ratio")

dfC %>%
  ggplot(aes(x=SRL_TL, fill=Size_Class))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="SRL_TL Ratio")

dfC %>%
  group_by(Age2) %>%
  shapiro_test(SRL_TL) %>%
  ungroup()

dfC %>%
  ggplot(aes(x=TL_ImgJ, fill=Age_Class))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="TL")

dfC %>%
  ggplot(aes(x=TL_ImgJ, fill=Age2))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="TL")

dfC %>%
  group_by(Age2) %>%
  shapiro_test(TL_ImgJ) %>%
  ungroup()

dfC %>%
  ggplot(aes(x=SRL_TRL, fill=Age_Class))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="SRL/TRL")

dfC %>%
  ggplot(aes(x=SRL_TRL, fill=Age2))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="SRL/TRL")

dfC %>%
  group_by(Age2) %>%
  shapiro_test(SRL_TRL) %>%
  ungroup()

####Residuals----
Cres$Publication <- as.factor(Cres$Publication)
Cres$Age_Class <- as.factor(Cres$Age_Class)
Cres$Maturity <- as.factor(Cres$Maturity)

ggqqplot(Cres$Residuals_SRL)
ggdensity(Cres$Residuals_SRL,
          main = "Density plot of Residuals",
          xlab = "Residuals - SRL")
shapiro.test(Cres$Residuals_SRL) #not normal

ggqqplot(Cres$TLCalc_SRL)
ggdensity(Cres$TLCalc_SRL,
          main = "Density plot of Residuals",
          xlab = "Residuals - SRL")
shapiro.test(Cres$TLCalc_SRL) #not normal

ggqqplot(Cres$NetRes_S)
ggdensity(Cres$NetRes_S,
          main = "Density plot of Residuals",
          xlab = "Residuals - SRL")
shapiro.test(Cres$NetRes_S) #not normal

ggqqplot(Cres$logResS)
ggdensity(Cres$logResS,
          main = "Density plot of Residuals",
          xlab = "Residuals - SRL")
shapiro.test(Cres$logResS) #not normal

ggqqplot(Cres$StResS)
ggdensity(Cres$StResS,
          main = "Density plot of Residuals",
          xlab = "Residuals - SRL")
shapiro.test(Cres$StResS) #not normal

ggqqplot(Cres$Residuals_TRL)
ggdensity(Cres$Residuals_TRL,
          main = "Density plot of Residuals",
          xlab = "Residuals - TRL")
shapiro.test(Cres$Residuals_TRL) #not normal

#Pub

dfCR %>%
  ggplot(aes(x=Residuals_SRL, fill=Publication))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Residuals - SRL")

dfCR %>%
  group_by(Publication) %>%
  shapiro_test(Residuals_SRL) %>%
  ungroup() #normal

dfCR %>%
  ggplot(aes(x=NetRes_S, fill=Publication))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Net Residuals")

dfCR %>%
  group_by(Publication) %>%
  shapiro_test(NetRes_S) %>%
  ungroup() #notnormal

dfCR %>%
  ggplot(aes(x=logResS, fill=Publication))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Residuals - Log")

dfCR %>%
  group_by(Publication) %>%
  shapiro_test(logResS) %>%
  ungroup()

dfCR %>%
  ggplot(aes(x=StResS, fill=Publication))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Residuals - Std")

dfCR %>%
  group_by(Publication) %>%
  shapiro_test(StResS) %>%
  ungroup()

#AgeClass

dfCR %>%
  ggplot(aes(x=Residuals_SRL, fill=Age_Class))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Residuals - SRL")

dfCR %>%
  group_by(Age_Class) %>%
  shapiro_test(Residuals_SRL) %>%
  ungroup()

dfCR %>%
  ggplot(aes(x=NetRes_S, fill=Age_Class))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Residuals - SRL")

dfCR %>%
  group_by(Age_Class) %>%
  shapiro_test(NetRes_S) %>%
  ungroup()

dfCR %>%
  ggplot(aes(x=logResS, fill=Age_Class))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Residuals - SRL")

dfCR %>%
  group_by(Age_Class) %>%
  shapiro_test(logResS) %>%
  ungroup()

dfCR %>%
  ggplot(aes(x=StResS, fill=Age_Class))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Residuals - SRL")

dfCR %>%
  group_by(Age_Class) %>%
  shapiro_test(StResS) %>%
  ungroup()

###Variance----
####Ratios----
PC$Age_Class <- as.factor(PC$Age_Class)
leveneTest(LJR_SRL ~ Age_Class, data = PC) #equal var
leveneTest(SRL_TL ~ Age_Class, data = PC) #equal var
leveneTest(SRL_TRL ~ Age_Class, data = PC) #equal var
leveneTest(TL_ImgJ ~ Age_Class, data = PC) #equal var

####Residuals----
Cres$Publication <- as.factor(Cres$Publication)
leveneTest(TLCalc_SRL ~ Publication*Age_Class, data = Cres) #equal var
leveneTest(TLCalc_SRL ~ Publication, data = Cres) #equal var

leveneTest(Residuals_SRL ~ Publication*Age_Class, data = Cres) #equal var
leveneTest(Residuals_SRL ~ Publication, data = Cres) #equal var

leveneTest(NetRes_S ~ Publication*Age_Class, data = Cres) #equal var
leveneTest(NetRes_S ~ Publication, data = Cres) #unequal var

leveneTest(StNetResS ~ Publication*Age_Class, data = Cres) #equal var
leveneTest(StNetResS ~ Publication, data = Cres) #unequal var

##Test Ratios----
####Ratio Reliability----
#accuracy of measurements on ratios - not sig.

aovC0 <- aov(LJR_SRL ~ TL_Real, data = PC) #1Way
summary(aovC0)
aovC1 <- aov(LJR_SRL ~ Rost_Real, data = PC) #1Way
summary(aovC1)
aovC2 <- aov(LJR_SRL ~ State, data = PC) #1Way
summary(aovC2)
aovC3 <- aov(LJR_SRL ~ Multiple_Planes, data = PC) #1Way
summary(aovC3)

aovC0 <- aov(SRL_TRL ~ TL_Real, data = PC) #1Way
summary(aovC0)
aovC1 <- aov(SRL_TRL ~ Rost_Real, data = PC) #1Way
summary(aovC1)
aovC2 <- aov(SRL_TRL ~ State, data = PC) #1Way
summary(aovC2)
aovC3 <- aov(SRL_TRL ~ Multiple_Planes, data = PC) #1Way
summary(aovC3)

aovC0 <- aov(SRL_TL ~ TL_Real, data = PC) #1Way
summary(aovC0)
aovC1 <- aov(SRL_TL ~ Rost_Real, data = PC) #1Way
summary(aovC1)
aovC2 <- aov(SRL_TL ~ State, data = PC) #1Way
summary(aovC2)
aovC3 <- aov(SRL_TL ~ Multiple_Planes, data = PC) #1Way
summary(aovC3)

t.test(formula = LJR_SRL~ Multiple_Planes, data = dfC)
t.test(formula = LJR_SRL~ TL_Real, data = dfC)
t.test(formula = SRL_TL~ Multiple_Planes, data = dfC)
t.test(formula = SRL_TL~ TL_Real, data = dfC)
t.test(formula = SRL_TRL~ Multiple_Planes, data = dfC)
t.test(formula = SRL_TRL~ TL_Real, data = dfC)

####Ratio by Age----
#visualise SRL to TL dep on age class (not significant)

#Means

PC %>%
  group_by(Age_Class) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(SRL_TRL, na.rm = TRUE),
    sd = sd(SRL_TRL, na.rm = TRUE)) %>%
  ungroup()

PC %>%
  group_by(Age_Class) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(SRL_TL, na.rm = TRUE),
    sd = sd(SRL_TL, na.rm = TRUE)) %>%
  ungroup()

aovC4 <- aov(LJR_SRL ~ Age_Class, data = PC) #1Way
summary(aovC4)
aovC5 <- aov(SRL_TL ~ Age_Class, data = PC) #1Way
summary(aovC5)
aovC6 <- aov(SRL_TRL ~ Age_Class, data = PC) #1Way
summary(aovC6)

t.test(formula = LJR_SRL ~ Maturity, data = dfC)
t.test(formula = SRL_TRL ~ Maturity, data = dfC)
t.test(formula = SRL_TL ~ Maturity, data = dfC)
t.test(formula = SRL_TL ~ Size_Class, data = dfC)

t.test(formula = SRL_TL ~ Age1, data = dfC)
t.test(formula = SRL_TL ~ Age2, data = dfC)
t.test(formula = SRL_TL ~ Age3, data = dfC)
t.test(formula = SRL_TL ~ Age4, data = dfC)
t.test(formula = SRL_TL ~ Age5, data = dfC)

#Between 1.5 and 1.6
#redefine and try again

PC_SRL <- PC$SRL
PC_TL <- PC$TL_ImgJ
cor(PC_SRL, PC_TL)

ggplot(PC, aes(x=SRL, y=TL_ImgJ, color=Age2)) + 
  geom_point()+
  geom_smooth(method=lm)

ggplot(PC, aes(x=Age_Class, y=LJR_SRL, color=Age_Class)) + 
  geom_boxplot() +
  #coord_flip() +
  labs(title = "LJR:SRL Clavata")

ggplot(PC, aes(x=Age2, y=LJR_SRL, color=Age2)) + 
  geom_boxplot() +
  #coord_flip() +
  labs(title = "LJR:SRL Clavata")

ggplot(PC, aes(x=Age_Class, y=SRL_TL, color=Age_Class)) + 
  geom_boxplot() +
  #coord_flip() +iol
  labs(title = "SRL_TL Clavata")

 ggplot(PC, aes(x=Age2, y=SRL_TL, color=Age2)) + 
  geom_boxplot() +
  #coord_flip() +
  labs(title = "SRL_TL Clavata")
 
 ####State----
 
 PC %>%
   group_by(State) %>% #means Residuals by group Pub
   dplyr::summarise(
     count = n(),
     mean = mean(SRL_TL, na.rm = TRUE),
     sd = sd(SRL_TL, na.rm = TRUE)) %>%
   ungroup()
 
 modelC1 = lm(SRL_TL ~ State + Maturity + State:Maturity, data = PC)
 Anova(modelC1, type = "II")
 
 modelC2 = lm(SRL_TL ~ State + Age_Class + State:Age_Class, data = PC)
 Anova(modelC2, type = "II")
 
##Isometry PC----
 
 model_C <- lm(log(SRL) ~ log(TL_ImgJ), data = PC)
 summary(model_C)
 
##TRL vs SRL----

#MEANS
 
 dfCR %>%
   group_by(Publication, Size_Class) %>% #means Residuals by group SRL
   dplyr::summarise(
     count = n(),
     mean = mean(StNetResS, na.rm = TRUE),
     sd = sd(StNetResS, na.rm = TRUE))%>%
   ungroup()

 dfCR %>%
   group_by(Publication, Size_Class) %>% #means Residuals by group SRL
   dplyr::summarise(
     count = n(),
     mean = mean(StNetResT, na.rm = TRUE),
     sd = sd(StNetResT, na.rm = TRUE))%>%
   ungroup()

#t tests

#overall
 
 dfCR %>%  #ttest - calc vs true SRL
   do(tidy(t.test(.$StResS,
                  .$StResT,
                  mu = 0,
                  alt = "two.sided",
                  paired = TRUE,
                  conf.level = 0.99)))
 
 dfCR %>%  #ttest - calc vs true SRL
   do(tidy(wilcox.test(.$TLCalc_SRL,
                  .$TLCalc_TRL,
                  mu = 0,
                  alt = "two.sided",
                  paired = TRUE,
                  conf.level = 0.99)))
#size/maturity
 
 dfCR %>%  #ttest - calc vs true SRL
   group_by(Size_Class) %>%
   do(tidy(wilcox.test(.$NetRes_S,
                       .$NetRes_T,
                       mu = 0,
                       alt = "two.sided",
                       paired = TRUE,
                       conf.level = 0.99)))
 ungroup()
 
 dfCR %>%  #ttest - calc vs true SRL
   group_by(Size_Class) %>%
   do(tidy(t.test(.$StResS,
                  .$StResT,
                  mu = 0,
                  alt = "less",
                  paired = TRUE,
                  conf.level = 0.99)))
 ungroup()
 
 #publication
 
 dfCR %>%  #ttest - calc vs true SRL
   group_by(Publication) %>%
   do(tidy(wilcox.test(.$NetRes_S,
                  .$NetRes_T,
                  mu = 0,
                  alt = "two.sided",
                  paired = TRUE,
                  exact = FALSE,
                  conf.level = 0.99))) %>%
   ungroup()
 
 dfCR %>%  #ttest - calc vs true SRL
   group_by(Publication) %>%
   do(tidy(t.test(.$StNetResS,
                       .$StNetResT,
                       mu = 0,
                       alt = "two.sided",
                       paired = TRUE,
                       exact = FALSE,
                       conf.level = 0.99))) %>%
   ungroup()
 
 dfCR %>%  #ttest - calc vs true SRL
   group_by(Publication) %>%
   do(tidy(wilcox.test(.$TLCalc_SRL,
                       .$TLCalc_TRL,
                  mu = 0,
                  alt = "two.sided",
                  paired = TRUE,
                  exact = FALSE,
                  conf.level = 0.99))) %>%
   ungroup()
 
 #interaction
 
 dfCR %>%  #ttest - calc vs true SRL
   group_by(Publication, Size_Class) %>%
   do(tidy(t.test(.$StNetResS,
                  .$StNetResT,
                  mu = 0,
                  alt = "greater",
                  paired = TRUE,
                  exact = FALSE,
                  conf.level = 0.99))) %>%
   ungroup()

 dfCR %>%  #ttest - calc vs true SRL
   group_by(Publication, Maturity) %>%
   do(tidy(t.test(.$StNetResS,
                  .$StNetResT,
                  mu = 0,
                  alt = "less",
                  paired = TRUE,
                  exact = FALSE,
                  conf.level = 0.99))) %>%
   ungroup()
 
####SRL TRL Barplot----
 #Stacked Bar
 
 CNRP <- data_summary(dfCR, varname = "NetRes_S", 
                      groupnames=c("Publication", "Maturity"))
 
 CNRP$Maturity=as.factor(CNRP$Maturity)
 head(CNRP)
 
 SNBarCR <- ggplot(CNRP, aes(x=NetRes_S,
                             y=Publication, 
                             fill=Maturity)) + 
   geom_bar(stat="identity", 
            position="stack") +
   labs(title = "P. clavata", x = "Publication", y = "Net Residuals") +
   theme(plot.title = element_text(face = "italic")) +
   theme_minimal() +
   scale_fill_manual(name = "Maturity",
                     labels=c('Immature', 'Mature'), 
                     values = c("grey90", "grey40"))
 SNBarCR
 
 #Okay now lets compare SRL to TRL
 
 CSNRP <- data_summary(dfCR, varname = "StNetResS", 
                       groupnames=c("Publication", "Maturity", "ModelT"))
 
 CSNRP$Maturity=as.factor(CSNRP$Maturity)
 head(CSNRP)
 
 StBarCR <- ggplot(CSNRP[order(CSNRP$Maturity,decreasing=T),],
                   aes(x=reorder(Publication, +StNetResS),
                       y=StNetResS, 
                       fill=factor(Maturity, levels = c("Mature", "Immature")),
                       pattern = ModelT)) + 
   geom_bar(stat="identity", 
            position="stack") +
   geom_bar_pattern(stat="identity",
                    position = "stack",
                    color = "black", 
                    pattern_fill = "black",
                    pattern_angle = 45,
                    pattern_density = 0.09,
                    pattern_spacing = 0.05,
                    pattern_key_scale_factor = 0.6) +
   scale_pattern_manual(values = c(Lin = "stripe", Pow = "none"),
                        labels = c('Linear', 'Power')) +
   geom_text(aes(label = paste(round(StNetResS,1),"±", round(sd,1))),
             position = position_stack(), hjust = 1.2, size = 3, fontface = 'bold', color = "white") +
   labs(title = "SRL Input", x = "Publication", y = "Standardised Net Residuals Produced", pattern = "Model Type") +
   theme_minimal() +
   guides(pattern = guide_legend(override.aes = list(fill = "white")),
          fill = guide_legend(override.aes = list(pattern = "none")))+
   theme(plot.title = element_text(face = "italic"), 
         legend.position = c(0.82, 0.25), 
         legend.background = element_rect(fill="white")) +
   coord_flip() +
   ylim(0, 47) +
   scale_x_discrete(labels = c('Faria et al. 2007. - Power',
                               expression(paste(bold('*Whitty et al. 2014 - Power'))),
                               expression(paste(bold('*Faria et al. 2007 - Linear'))),
                               expression(paste(bold('Thorburn et al. 2008'))),
                               'Whitty et al. 2014 - Ratio',
                               'Peverell 2010')) +
   scale_fill_manual(name = "Maturity",
                     labels=c('Mature', 'Immature'), 
                     values = c("grey40", "grey70"))
 StBarCR
 
 CSNRT <- data_summary(dfCR, varname = "StNetResT", 
                       groupnames=c("Publication", "Maturity", "ModelT"))
 
 CSNRT$Maturity=as.factor(CSNRT$Maturity)
 head(CSNRT)
 
 StBarCRT <- ggplot(CSNRT[order(CSNRT$Maturity,decreasing=T),],
                    aes(x=factor(Publication, level = c("Faria Power",
                                                        "Whitty Power",
                                                        "Faria Linear",
                                                        "Thorburn",
                                                        "Whitty Linear",
                                                        "Peverell")), 
                        y=StNetResT, 
                        fill=factor(Maturity, levels = c("Mature", "Immature")),
                        pattern = ModelT)) + 
   geom_bar(stat="identity", 
            position="stack") +
   geom_bar_pattern(stat="identity",
                    position = "stack",
                    color = "black", 
                    pattern_fill = "black",
                    pattern_angle = 45,
                    pattern_density = 0.09,
                    pattern_spacing = 0.05,
                    pattern_key_scale_factor = 0.6) +
   scale_pattern_manual(values = c(Lin = "stripe", Pow = "none"),
                        labels = c('Linear', 'Power')) +
   geom_text(aes(label = paste(round(StNetResT,1),"±", round(sd,1))),
             position = position_stack(), hjust = 1.2, size = 3, fontface = 'bold', color = "white") +
   labs(title = "TRL Input", x = "Publication", y = "Standardised Net Residuals Produced", pattern = "Model Type") +
   theme_minimal() +
   guides(pattern = guide_legend(override.aes = list(fill = "white")),
          fill = guide_legend(override.aes = list(pattern = "none")))+
   theme(plot.title = element_text(face = "italic"), 
         legend.position =c(0.82, 0.4),
         legend.background = element_rect(fill="white")) +
   coord_flip() +
   ylim(0, 47) +
   scale_x_discrete(labels = c('Faria et al. 2007. - Power',
                               expression(paste(bold('*Whitty et al. 2014 - Power'))),
                               expression(paste(bold('*Faria et al. 2007 - Linear'))),
                               expression(paste(bold('Thorburn et al. 2008'))),
                               'Whitty et al. 2014 - Ratio',
                               'Peverell 2010')) +
   scale_fill_manual(name = "Maturity",
                     labels=c('Mature', 'Immature'), 
                     values = c("grey40", "grey70"))
 StBarCRT
 
 #TRL vs SRL - Pristis C discussion
 cowplot::plot_grid(StBarCR + theme(axis.text.x = element_blank(),
                                    axis.title.x = element_blank(),
                                    legend.position = "none"),
                    StBarCRT,
                    labels = "AUTO", ncol = 1, align ="v",
                    rel_heights = c(.85,1))
 
 ggsave(
   "ClavataTvS.tiff",
   plot = last_plot(),
   device = NULL,
   path = NULL,
   scale = 1,
   width = 10,
   height = 6,
   units = c("in", "cm", "mm", "px"),
   dpi = 300,
   limitsize = TRUE,
   bg = NULL,
 )
 
##Residuals by Age----
sumtable(Cres, group = "Publication", group.long=T) #as Table

####Mean Resids----
library("dplyr")
Cres %>%
  group_by(Age_Class) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(Residuals_SRL, na.rm = TRUE),
    sd = sd(Residuals_SRL, na.rm = TRUE))%>%
  ungroup()

library("dplyr")
Cres %>%
  group_by(Age_Class) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(NetRes_S, na.rm = TRUE),
    sd = sd(NetRes_S, na.rm = TRUE))%>%
  ungroup()

Cres %>%
  group_by(Age_Class) %>% #means Residuals by group TRL
  dplyr::summarise(
    count = n(),
    mean = mean(Residuals_TRL, na.rm = TRUE),
    sd = sd(Residuals_TRL, na.rm = TRUE))%>%
ungroup()

Cres %>%
  group_by(Age_Class) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(SSq_SRL, na.rm = TRUE),
    sd = sd(SSq_SRL, na.rm = TRUE))%>%
  ungroup()

Cres %>%
  group_by(Age_Class) %>% #means Residuals by group TRL
  dplyr::summarise(
    count = n(),
    mean = mean(SSq_TRL, na.rm = TRUE),
    sd = sd(SSq_TRL, na.rm = TRUE))%>%
  ungroup()


####test Age----
wilcox.test(Residuals_SRL ~ Maturity, Cres)
wilcox.test(NetRes_S ~ Maturity, Cres)
wilcox.test(logResS ~ Maturity, Cres)

wtCSRLAge <- Cres %>% #wilcoxtest
  group_by(Age_Class) %>%
  do(tidy(wilcox.test(.$TLCalc_SRL,
                      .$TLTrue,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      conf.level = 0.99))) %>%
  ungroup()
wtCSRLAge

Cres %>% #wilcoxtest
  group_by(Age_Class) %>%
  do(tidy(wilcox.test(.$TLCalc_SRL,
                      .$TLCalc_TRL,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      exact = FALSE,
                      conf.level = 0.99))) %>%
  ungroup()

Cres %>% #wilcoxtest
  group_by(Publication) %>%
  do(tidy(wilcox.test(.$Residuals_SRL,
                      .$Residuals_TRL,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      exact = FALSE,
                      conf.level = 0.99))) %>%
  ungroup()

###ANOVA/Krusk Age----

aovCAge <- aov(Residuals_SRL ~ Age_Class, Cres)
summary(aovCAge)

kruskal.test(Residuals_SRL ~ Age_Class, data = Cres)
kruskal.test(logResS ~ Age_Class, data = Cres)
kruskal.test(NetRes_S ~ Age_Class, data = Cres)

####Boxplot----
ggplot(Cres, aes(Age_Class, Residuals_SRL, color=Age_Class)) +
  geom_boxplot() +
  #coord_flip() +
  labs(title = "Projected Total Length of Clavata SRL ", 
       x= bquote(NULL),
       y= bquote("Residuals"))

ggplot(Cres, aes(Age2, Residuals_SRL, color=Age2)) +
  geom_boxplot() +
  #coord_flip() +
  labs(title = "Projected Total Length of Clavata SRL ", 
       x= bquote(NULL),
       y= bquote("Residuals"))

ggplot(Cres, aes(Age_Class, Residuals_TRL, color=Age_Class)) +
  geom_boxplot() +
  #coord_flip() +
  labs(title = "Projected Total Length of Clavata TRL ", 
       x= bquote(NULL),
       y= bquote("Residuals"))

##Residuals by Pub----
sumtable(Cres, group = "Publication", group.long=T) #as Table

####t-test Pub----
Cres %>%
  group_by(Publication) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(Residuals_SRL, na.rm = TRUE),
    sd = sd(Residuals_SRL, na.rm = TRUE))%>%
  ungroup()

Cres %>%
  group_by(Publication) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(NetRes_S, na.rm = TRUE),
    sd = sd(NetRes_S, na.rm = TRUE))%>%
  ungroup()

Cres %>%
  group_by(Publication) %>% #means Residuals by group TRL
  dplyr::summarise(
    count = n(),
    mean = mean(Residuals_TRL, na.rm = TRUE),
    sd = sd(Residuals_TRL, na.rm = TRUE))%>%
  ungroup()

pwCLSRL <- Cres %>%  #ttest - calc vs 0
  group_by(Publication) %>%
  do(tidy(t.test(.$logResS,
                 mu = 0,
                 alt = "two.sided",
                 conf.level = 0.99)))%>%
  ungroup()
pwCLSRL

pwCStRL <- Cres %>%  #ttest - calc vs 0
  group_by(Publication) %>%
  do(tidy(t.test(.$StResS,
                 mu = 0,
                 alt = "two.sided",
                 conf.level = 0.99)))%>%
  ungroup()
pwCStRL

wtCSRLPub <- Cres %>% #wilcoxtest
  group_by(Publication, Size_Class) %>%
  do(tidy(wilcox.test(.$TLCalc_SRL,
                      .$TLTrue,
                      mu = 0,
                      exact = FALSE,
                      alt = "two.sided",
                      paired = TRUE,
                      conf.level = 0.99))) %>%
  ungroup()
wtCSRLPub

wtCTRLPub <- Cres %>% #wilcoxtest
  group_by(Publication) %>%
  do(tidy(wilcox.test(.$TLCalc_SRL,
                      .$TLTrue,
                      mu = 0,
                      exact = FALSE,
                      alt = "two.sided",
                      paired = TRUE,
                      conf.level = 0.99))) %>%
  ungroup()
wtCTRLPub

####ANOVA Pub----
aovCPUB <- aov(logResS ~ Age_Class + Age_Class*Publication, Cres)
summary(aovCPUB)
aovC2PUB <- aov(logResS ~ Publication + Maturity + Publication*Maturity, Cres) #v significant!
summary(aovC2PUB)
aovC3PUB <- aov(StNetResS ~ Age_Class + Age_Class*Publication, Cres)
summary(aovC3PUB)
aovC4PUB <- aov(StNetResS ~ Publication + Maturity + Publication*Maturity, Cres) #StNet takes away maturity being a factor
summary(aovC4PUB)

kruskal.test(NetRes_S ~ Publication, Cres)

wilcox.test(Ctest$NetFL, Ctest$NetFP,
            alt = "two.sided",
            exact = FALSE,
            conf.level = 0.99)

t.test(Ctest$LogFL, Ctest$LogFP,
            alt = "two.sided",
            conf.level = 0.99)

t.test(Ctest$SNStFL, Ctest$SNStFP,
       alt = "two.sided",
       conf.level = 0.99)

wilcox.test(Ctest$TL_FL_SRL, Ctest$TL_FP_SRL,
            alt = "two.sided",
            paired = TRUE,
            exact = FALSE,
            conf.level = 0.99)

wilcox.test(Ctest$NetWC, Ctest$NetWL,
            alt = "two.sided",
            exact = FALSE,
            conf.level = 0.99)

t.test(Ctest$LogWC, Ctest$LogWL,
       alt = "two.sided",
       conf.level = 0.99)

t.test(Ctest$SNStWC, Ctest$SNStWL,
       alt = "two.sided",
       conf.level = 0.99)

wilcox.test(Ctest$TL_WP_SRL, Ctest$TL_WL_SRL,
            alt = "two.sided",
            paired = TRUE,
            exact = FALSE,
            conf.level = 0.99)

####Resid Corr----
sapply(
  split(data.frame(Cres$TLTrue, Cres$TLCalc_SRL), Cres$Publication), 
  function(x) cor(x[[1]],x[[2]])
)

PCDPT <- ggplot(Cres, mapping=aes(x=TLTrue, y=StResT, color=Publication)) +
  geom_point() +
  geom_hline(yintercept=0,linetype="dashed") +
  scale_color_manual(labels=c('Faria (2007) Linear', 
                              'Faria (2007) Exponential',
                              'Peverell (2010)',
                              'Thorburn et al. (2008)',
                              'Whitty et al. (2014) Ratio',
                              'Whitty et al. (2014) Power'),
                     values = c("red3","#9900FF", "yellow", "green3", "orange2", "#003399")) +
  stat_ellipse(aes(x=TLTrue, y=StResT, color=Publication),type = "norm") +
  theme(plot.title = element_text(face = "italic")) +
  labs(title = "Pristis clavata",
       x = bquote("Total Length (mm)"),
       y = bquote("St. Residuals (% Error)"),
       fill = "Publication")
PCDPT

##Association Age-Pub----
####Means----
CAP1<- Cres %>%
  group_by(Publication, Age_Class) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(NetRes_S, na.rm = TRUE),
    sd = sd(NetRes_S, na.rm = TRUE)) %>%
  ungroup()
print(CAP1, n = Inf)

CAP2<- Cres %>%
  group_by(Publication, Age_Class) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResT, na.rm = TRUE),
    sd = sd(StNetResT, na.rm = TRUE)) %>%
  ungroup()
print(CAP2, n = Inf)

CAP3<- Cres %>%
  group_by(Publication, Age_Class) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)) %>%
  ungroup()
print(CAP3, n = Inf)

Cres %>%
  group_by(Publication, Maturity) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(NetRes_S, na.rm = TRUE),
    sd = sd(NetRes_S, na.rm = TRUE)) %>%
  ungroup()

Cres %>%
  group_by(Publication, Maturity) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResT, na.rm = TRUE),
    sd = sd(StNetResT, na.rm = TRUE)) %>%
  ungroup()

Cres %>%
  group_by(Maturity, Publication) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)) %>%
  ungroup()

Cres %>%
  group_by(Size_Class, Publication) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)) %>%
  ungroup()

Cres %>%
  group_by(Publication, Size_Class) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)) %>%
  ungroup()

Cres %>%
  group_by(Publication, Size_Class) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(NetRes_S, na.rm = TRUE),
    sd = sd(NetRes_S, na.rm = TRUE)) %>%
  ungroup()

####t-Test split----
t.test(formula = SNStP ~ Size_Class, data = dfCT) #notsignificant
t.test(formula = SNStFL ~ Size_Class, data = dfCT) #notsignificant
t.test(formula = SNStFP ~ Size_Class, data = dfCT) #notsignificant
t.test(formula = SNStWC ~ Size_Class, data = dfCT) #notsignificant
t.test(formula = SNStWL ~ Size_Class, data = dfCT) #notsignificant
t.test(formula = SNStT ~ Size_Class, data = dfCT) #notsignificant

wilcox.test(formula = NetP ~ Size_Class, data = dfCT) #significant
wilcox.test(formula = NetFL ~ Size_Class, data = dfCT) #notsignificant
wilcox.test(formula = NetFP ~ Size_Class, data = dfCT) #notsignificant
wilcox.test(formula = NetWC ~ Size_Class, data = dfCT) #notsignificant
wilcox.test(formula = NetWL ~ Size_Class, data = dfCT) #notsignificant
wilcox.test(formula = NetT ~ Size_Class, data = dfCT) #significant

wilcox.test(formula = NetP ~ Maturity, data = dfCT)
wilcox.test(formula = NetFL ~ Maturity, data = dfCT)
wilcox.test(formula = NetFP ~ Maturity, data = dfCT)
wilcox.test(formula = NetWC ~ Maturity, data = dfCT)
wilcox.test(formula = NetWL ~ Maturity, data = dfCT)
wilcox.test(formula = NetT ~ Maturity, data = dfCT)

t.test(formula = SNStP ~ Maturity, data = dfCT)
t.test(formula = SNStFL ~ Maturity, data = dfCT)
t.test(formula = SNStFP ~ Maturity, data = dfCT)
t.test(formula = SNStWC ~ Maturity, data = dfCT)
t.test(formula = SNStWL ~ Maturity, data = dfCT)
t.test(formula = SNStT ~ Maturity, data = dfCT)

####ANOVA Split by Pubs----

kruskal.test(NetP ~ Age_Class, data = Ctest) #1Way
kruskal.test(NetFL ~ Age_Class, data = Ctest) #1Way
kruskal.test(NetFP ~ Age_Class, data = Ctest) #1Way
kruskal.test(NetWL ~ Age_Class, data = Ctest) #1Way
kruskal.test(NetWC ~ Age_Class, data = Ctest) #1Way
kruskal.test(NetT ~ Age_Class, data = Ctest) #1Way

kruskal.test(SNStP ~ Age_Class, data = Ctest) #1Way
kruskal.test(SNStFL ~ Age_Class, data = Ctest) #1Way
kruskal.test(SNStFP ~ Age_Class, data = Ctest) #1Way
kruskal.test(SNStWL ~ Age_Class, data = Ctest) #1Way
kruskal.test(SNStWC ~ Age_Class, data = Ctest) #1Way
kruskal.test(SNStT ~ Age_Class, data = Ctest) #1Way

aovCM <- aov(StNetResS ~ ModelT + Maturity + ModelT:Maturity, data = Cres)
summary(aovCM)

####Boxplot Age-Pub----
ggplot(Cres, aes(Age_Class, Residuals_SRL, color=Publication)) +
  geom_boxplot() +
  #coord_flip() +
  labs(title = "Projected Total Length of Sawfishes ", 
       x= bquote(NULL),
       y= bquote("Residuals"))
##Gradient----

modCP <- lm(SStP ~ TLCorr, data = Ctest)
modCFL <- lm(SStFL ~ TLCorr, data = Ctest)
modCFP <- lm(SStFP~ TLCorr, data = Ctest)
modCT <- lm(SStT~ TLCorr, data = Ctest)
modCWL <- lm(SStWL~ TLCorr, data = Ctest)
modCWP<- lm(SStWC~ TLCorr, data = Ctest)

modelsCPub <- list(modCP, modCFL, modCFP, modCT, modCWL, modCWP)
modCnamP <- c('P', 'FL', 'FP', 'T', 'WL', 'WP')

summary(modCP)
summary(modCFL)
summary(modCFP)
summary(modCT)
summary(modCWL)
summary(modCWP)

##ToPrint----
####BarPlot Pub-Age----
CRP <- data_summary(Cres, varname = "Residuals_SRL", 
                    groupnames=c("Publication", "Age_Class"))

CRP$Age_Class=as.factor(CRP$Age_Class)
head(CRP)

PCwrap <- c("Whitty et al. (2014) Power",
            "Faria (2007) Power",
            "Faria (2007) Linear",
            "Thorburn et al. (2008)",
            "Whitty et al. (2014) Ratio",
            "Peverell (2010)")

wrap_labelsC <- function(x) {
  str_wrap(PCwrap, width = 15)
}

BarCR1 <- ggplot(CRP, aes(x=factor(Publication, level = c("Whitty Power",
                                                         "Faria Power",
                                                         "Faria Linear",
                                                         "Thorburn",
                                                         "Whitty Linear",
                                                         "Peverell")), 
                         y=Residuals_SRL,
                         fill=factor(Age_Class, level = c('YOY',
                                                          'juvenile',
                                                          'sub-adult',
                                                          'adult')))) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Residuals_SRL-sd, ymax=Residuals_SRL+sd), width=.2,
                position=position_dodge(.9)) +
  labs(title = "P. clavata", x = "Model", y = "Residuals (mm)") +
  theme(plot.title = element_text(face = "italic"),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  scale_fill_manual(name = "Size Class",
                    labels = c('YOY', 'Juvenile', 'Sub-adult', 'Adult'), 
                    values = c("#5ab4ac", "#f5f5f5", "56B4E9", "#d8b365")) + 
  scale_x_discrete(labels = c('W', 'FP', 'FL', 'T', 'WR', 'P')) 
print(BarCR1)
# Finished bar plot

####DotPlot----
Cres$Publication <-as.factor(Cres$Publication)

PCDP <- ggplot(Cres, mapping=aes(x=TLTrue, 
                                 y=StResS, 
                                 color=factor(Publication,
                                              levels = c('Thorburn',
                                              'Faria Power', 
                                              'Whitty Power',
                                              'Faria Linear',
                                              'Peverell',
                                              'Whitty Linear'
                                              )))) +
  coord_cartesian(xlim =c(500, 5050),
                  ylim = c(-30, 45)) +
  geom_point(size = 0.75) +
  geom_hline(yintercept = c(-5, 0, 5),
             linetype = c('dotted', "longdash", 'dotted'),
             color = c('grey40', 'black', 'grey40')) +
  annotate("rect", xmin = -Inf, xmax = Inf, 
           ymin = -5.2, ymax= 5.2, alpha = 0.1, fill = 'grey40') +
  scale_color_manual(name = 'Model',
                     labels=c('Thorburn et al. (2008)',
                              'Faria (2007) Power',
                              'Whitty et al. (2014) Power',
                              'Faria (2007) Linear',
                              'Peverell (2010)',
                              'Whitty et al. (2014) Ratio'),
                     values = c('plum',
                                'yellow',
                                'lightblue',
                                'orange',
                                rgb(0.2,0.4,0.1,0.7),
                                'darkblue')) +
  stat_smooth(aes(x=TLTrue, y=StResS, color=Publication), 
              method = lm, se = FALSE) +
  geom_vline(xintercept = c(810, 1100, 1950, 2600, 5080),
             linetype = c('solid', 'solid', 'dotdash', 'solid', 'solid')) +
  # annotate(geom = "text",
  #          label = c('Birth Size', 'One Year', ' ', 'Maturity', 'Max Size'),
  #          x = c(860, 1140, 1990, 2640, 5120),
  #          y = c(38, 38, 38, 38, 38),
  #          size = c(3.5,3.5,3.5,3.5,3.5),
  #          angle = 90,
  #          vjust = -1) +
  geom_segment(x = 810, y = 48, xend = 1100, yend = 48,
               colour = "#5ab4ac",
               size = 3) +
  geom_segment(x = 1100, y = 48, xend = 1950, yend = 48,
               colour = "#f5f5f5",
               size = 3) +
  geom_segment(x = 1950, y = 48, xend = 2600, yend = 48,
               colour = "lightgrey",
               size = 3) +
  geom_segment(x = 2600, y = 48, xend = 5080, yend = 48,
               colour = "#d8b365",
               size = 3) +
  # annotate(geom = "label",
  #          label = c('YOY', 'Juvenile', 'Sub-Adult', 'Adult'),
  #          x = c((810+1100)/2, (1100+1950)/2, (1950+2600)/2, (2600+5080)/2),
  #          y = c(57.5, 57.5, 57.5, 57.5),
  #          size = c(4,4,4,4),
  #          fill = c("#5ab4ac", "#f5f5f5", "lightgrey", "#d8b365"),
  #          colour = c("white", "black", "black", "white"),
  #          angle = 0,
  #          vjust = 1) +
  theme(plot.title = element_text(size = 14, face = "italic"),
        legend.key = element_rect(fill = "white"),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "bottom") +
  labs(title = "Pristis clavata",
       x = bquote("Total Length (mm)"),
       y = bquote("Percent Error (%)"),
       fill = "Model")
PCDP

####Pub Section Pic----
#Dot + Box

cowplot::plot_grid(BarCR + theme(plot.title = element_blank(),
                                 legend.position = "none"),
                   PCDP + theme(plot.title = element_blank()),
                   labels = "auto", ncol = 1, rel_heights = c(1,2), align ="v")

ggsave(
  "ClavatDat.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 10,
  height = 7.5,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

##Appx: Residual Plot----
####Big Boi----
library(cowplot)
Ca <- ggplot(dfCT, aes(x = SRL, y = TLCorr)) + #Pev
  geom_smooth(aes(x = SRL, y = TL_Pev_SRL), method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = SRL, yend = TL_Pev_SRL), alpha = .2) +
  ylim(0, 4000) +
  geom_point(aes(color = ResidP)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  geom_point(aes(y = TLCorr), shape = 1) +
  labs(x= "SRL", y = "Total Length", 
       title= "Peverell, 2010", color ="Residuals") +
  theme_bw()
Cb <- ggplot(dfCT, aes(x = SRL, y = TLCorr)) + #FariaL
  geom_smooth(aes(x = SRL, y = TL_FL_SRL), method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = SRL, yend = TL_FL_SRL), alpha = .2) +
  ylim(0, 4000) +
  geom_point(aes(color = ResidFL)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  geom_point(aes(y = TLCorr), shape = 1) +
  labs(x= "SRL", y = "Total Length", 
       title= "Faria, 2007 - Linear", color ="Residuals") +
  theme_bw()
Cc <- ggplot(dfCT, aes(x = SRL, y = TLCorr)) + #FariaE
  stat_smooth(aes(x = SRL, y = TL_FP_SRL), method = "loess", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = SRL, yend = TL_FP_SRL), alpha = .2) +
  ylim(0, 4000) +
  geom_point(aes(color = ResidFP)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  geom_point(aes(y = TLCorr), shape = 1) +
  labs(x= "SRL", y = "Total Length", 
       title= "Faria, 2007 - Power", color ="Residuals") +
  theme_bw()
Cd <- ggplot(dfCT, aes(x = SRL, y = TLCorr)) + #WhittyE
  stat_smooth(aes(x = SRL, y = TL_WP_SRL), method = "loess", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = SRL, yend = TL_WP_SRL), alpha = .2) +
  ylim(0, 4000) +
  geom_point(aes(color = ResidWC)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  geom_point(aes(y = TLCorr), shape = 1) +
  labs(x= "SRL", y = "Total Length", 
       title= "Whitty et al. 2014 - Power", color ="Residuals") +
  theme_bw()
Ce <- ggplot(dfCT, aes(x = SRL, y = TLCorr)) + #WhittyL
  geom_smooth(aes(x = SRL, y = TL_WL_SRL), method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = SRL, yend = TL_WL_SRL), alpha = .2) +
  ylim(0, 4000) +
  geom_point(aes(color = ResidWL)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  geom_point(aes(y = TLCorr), shape = 1) +
  labs(x= "SRL", y = "Total Length", 
       title= "Whitty et al. 2014 - Ratio", color ="Residuals") +
  theme_bw()
Cf <- ggplot(dfCT, aes(x = SRL, y = TLCorr)) + #Thor
  geom_smooth(aes(x = SRL, y = TL_Thor_SRL), method = "loess", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = SRL, yend = TL_Thor_SRL), alpha = .2) +
  ylim(0, 4000) +
  geom_point(aes(color = ResidT)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  geom_point(aes(y = TLCorr), shape = 1) +
  labs(x= "SRL", y = "Total Length", 
       title= "Thorburn et al. 2008", color ="Residuals") +
  theme_bw()

CResGALL <- cowplot::plot_grid(Cb, 
                               Ce + theme(axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank(),
                                          axis.title.y = element_blank() ),
                               Ca + theme(axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank(),
                                          axis.title.y = element_blank() ),
                               Cc,
                               Cd + theme(axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank(),
                                          axis.title.y = element_blank() ),
                               Cf + theme(axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank(),
                                          axis.title.y = element_blank() ),
                               labels = "AUTO", ncol = 3, align ="v")
CResGALL

ggsave(
  "ClavataRes.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 10,
  height = 5,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

###Model Type----

interaction.plot(x.factor     = Cres$ModelT,
                 trace.factor = Cres$Maturity,
                 response     = Cres$StNetResS,
                 fun = mean,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")

modelCT = lm(StNetResS ~ ModelT + Maturity + ModelT:Maturity, data = Cres)
Anova(modelCT, type = "II")

interaction.plot(x.factor     = Cres$ModelT,
                 trace.factor = Cres$Size_Class,
                 response     = Cres$NetRes_S,
                 fun = mean,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")

modelCM = lm(StNetResS ~ ModelT + Size_Class + ModelT:Size_Class, data = Cres)
Anova(modelCM, type = "II")

interaction.plot(x.factor     = factor(Cres$Age_Class, levels = c("YOY", "juvenile", "sub-adult", "adult")),
                 trace.factor = Cres$Publication,
                 response     = Cres$StNetResS,
                 fun = mean,
                 type="b",
                 col=c("black",
                       "red",
                       "purple", 
                       "blue",
                       "yellow", 
                       "orange"),  ### Colors for levels of trace var.
                 pch=c(19, 15, 15, 19, 19, 15),  ### Symbols for levels of trace var.
                 fixed=TRUE,                         ### Order by factor order in data
                 leg.bty = "o")

#PristisTest----
###Normality----
####Ratios----
library(car)
library(ggpubr)

ggqqplot(PP$LJR_SRL)
ggdensity(PP$LJR_SRL,
          main = "Density plot of LJR/SRL",
          xlab = "LJR/SRL")
shapiro.test(PP$LJR_SRL) #almostnormal

ggqqplot(PP$SRL_TL)
ggdensity(PP$SRL_TL,
          main = "Density plot of SRL/TL",
          xlab = "SRL_TL")
shapiro.test(PP$SRL_TL) #almostnormal

ggqqplot(PP$SRL_TRL)
ggdensity(PP$SRL_TRL,
          main = "Density plot of SRL/TRL",
          xlab = "SRL_TL")
shapiro.test(PP$SRL_TRL) #normal

ggqqplot(PP$TL_ImgJ)
ggdensity(PP$TL_ImgJ,
          main = "Density plot of TL",
          xlab = "TL")
shapiro.test(PP$TL_ImgJ) #notnormal

dfP %>%
  ggplot(aes(x=LJR_SRL, fill=Age_Class))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="LJR/SRL Ratio")

dfP %>%
  ggplot(aes(x=LJR_SRL, fill=AgeC3))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="LJR/SRL Ratio")

dfP %>%
  group_by(Maturity) %>%
  shapiro_test(LJR_SRL) %>%
  ungroup()

dfP %>%
  ggplot(aes(x=SRL_TL, fill=Age_Class))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="SRL_TL Ratio")

dfP %>%
  ggplot(aes(x=SRL_TL, fill=AgeC3))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="SRL_TL Ratio")

dfP %>%
  group_by(Maturity) %>%
  shapiro_test(SRL_TL) %>%
  ungroup()

dfP %>%
  ggplot(aes(x=TL_ImgJ, fill=Age_Class))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="TL")

dfP %>%
  ggplot(aes(x=TL_ImgJ, fill=AgeC2))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="TL")

dfP %>%
  group_by(Maturity) %>%
  shapiro_test(TL_ImgJ) %>%
  ungroup()

dfP %>%
  ggplot(aes(x=SRL_TRL, fill=Age_Class))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="SRL/TRL")


####Residuals----
ggqqplot(Ptest$TLCorr)
ggdensity(Ptest$TLCorr,
          main = "Density plot of Residuals",
          xlab = "Residuals - SRL")
shapiro.test(Ptest$TLCorr) #not normal

ggqqplot(Pres$Residuals_SRL)
ggdensity(Pres$Residuals_SRL,
          main = "Density plot of Residuals",
          xlab = "Residuals - SRL")
shapiro.test(Pres$Residuals_SRL) #not normal

ggqqplot(Pres$Residuals_TRL)
ggdensity(Pres$Residuals_TRL,
          main = "Density plot of Residuals",
          xlab = "Residuals - TRL")
shapiro.test(Pres$Residuals_TRL) #not normal

ggqqplot(Pres$NetRes_S)
ggdensity(Pres$NetRes_S,
          main = "Density plot of Residuals",
          xlab = "Residuals - SRL")
shapiro.test(Pres$NetRes_S) #not normal

ggqqplot(Pres$logResS)
ggdensity(Pres$logResS,
          main = "Density plot of Residuals",
          xlab = "Residuals - SRL")
shapiro.test(Pres$logResS) #not normal

ggqqplot(Pres$StNetResS)
ggdensity(Pres$StNetResS,
          main = "Density plot of Residuals",
          xlab = "Residuals - SRL")
shapiro.test(Pres$StNetResS) #not normal

ggqqplot(Pres$StResS)
ggdensity(Pres$StResS,
          main = "Density plot of Residuals",
          xlab = "Residuals - SRL")
shapiro.test(Pres$StResS) #not normal

ggqqplot(AllR$logNetSRL)

dfPR %>%
  ggplot(aes(x=Residuals_SRL, fill=Publication))+ #looksgood
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Residuals - SRL")

 dfPR %>%
  group_by(Publication) %>%
  shapiro_test(Residuals_SRL) %>%
  ungroup()

dfPR %>%
  ggplot(aes(x=NetRes_S, fill=Publication))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Residuals - SRL")

dfPR %>%
  group_by(Publication) %>%
  shapiro_test(NetRes_S) %>%
  ungroup()

dfPR %>%
  ggplot(aes(x=StResS, fill=Publication))+ #looksgood
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Residuals - SRL")

dfPR %>%
  group_by(Publication) %>%
  shapiro_test(StResS) %>%
  ungroup()

dfPR %>%
  ggplot(aes(x=logResS, fill=Publication))+ #nope
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Residuals - SRL")

dfPR %>%
  group_by(Publication) %>%
  shapiro_test(logResS) %>%
  ungroup()

#AgeClass

dfPR %>%
  ggplot(aes(x=Residuals_SRL, fill=Age_Class))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Residuals - SRL")

dfPR %>%
  group_by(Age_Class) %>%
  shapiro_test(Residuals_SRL) %>%
  ungroup()

dfPR %>%
  group_by(Age_Class) %>%
  shapiro_test(NetRes_S) %>%
  ungroup()

dfPR %>%
  ggplot(aes(x=NetRes_S, fill=Age_Class))+ 
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Residuals - SRL")

dfPR %>%
  group_by(Age_Class) %>%
  shapiro_test(logResS) %>%
  ungroup()

dfPR %>%
  ggplot(aes(x=logResS, fill=Age_Class))+ 
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Residuals - SRL")

dfPR %>%
  ggplot(aes(x=StResS, fill=Age_Class))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Standard Res")

dfPR %>%
  group_by(Age_Class) %>%
  shapiro_test(StResS) %>%
  ungroup()

###Variance----
####Ratios----
PP$Age_Class <- as.factor(PP$Age_Class)
leveneTest(LJR_SRL ~ Age_Class, data = PP) #equal var
leveneTest(SRL_TL ~ Age_Class, data = PP) #equal var
leveneTest(SRL_TRL ~ Age_Class, data = PP) #equal var
leveneTest(TL_ImgJ ~ Age_Class, data = PP) #unequal var

####Residuals----
Pres$Publication <- as.factor(Pres$Publication)

leveneTest(TLCalc_SRL ~ Publication, data = Pres) #equal var
leveneTest(TLCalc_TRL ~ Publication, data = Pres) #equal var

leveneTest(Residuals_SRL ~ Publication*Age_Class, data = Pres) #unequal var
leveneTest(Residuals_SRL ~ Publication, data = Pres) #equal var
leveneTest(Residuals_SRL ~ Age_Class, data = Pres) #unequal var

leveneTest(NetRes_S ~ Publication*Age_Class, data = Pres) #unequal var
leveneTest(NetRes_S ~ Publication, data = Pres) #equal var
leveneTest(NetRes_S ~ Age_Class, data = Pres) #unequal var

leveneTest(logResS ~ Publication*Age_Class, data = Pres) #unequal var
leveneTest(logResS ~ Publication, data = Pres) #equal var
leveneTest(logResS ~ Age_Class, data = Pres) #unequal var

##Test Ratios----
####Ratio Reliability----
#accuracy of measurements on ratios - not sig.

aovP1 <- aov(LJR_SRL ~ State, data = PP) #1Way
summary(aovP1)
aovP2 <- aov(SRL_TRL ~ State, data = PP) #1Way
summary(aovP2)
aovP3 <- aov(SRL_TL ~ State, data = PP) #1Way
summary(aovP3)

t.test(formula = LJR_SRL~ Multiple_Planes, data = dfP)
t.test(formula = LJR_SRL~ TL_Real, data = dfP)
t.test(formula = LJR_SRL~ Rost_Real, data = dfP)
wilcox.test(formula = LJR_SRL ~ TL_Real, data = dfP)

t.test(formula = SRL_TRL~ Multiple_Planes, data = dfP)
t.test(formula = SRL_TRL~ TL_Real, data = dfP)
t.test(formula = SRL_TRL~ Rost_Real, data = dfP)

t.test(formula = SRL_TL~ Multiple_Planes, data = dfP)
t.test(formula = SRL_TL~ TL_Real, data = dfP)
t.test(formula = SRL_TL~ Rost_Real, data = dfP)
wilcox.test(formula = SRL_TL ~ TL_Real, data = dfP)

####Ratio by Age----

aovP5 <- aov(SRL_TL ~ Age_Class, data = PP) 
summary(aovP5)
kruskal.test(SRL_TL ~ Age_Class, data = PP) #SIGNIFICANT
kruskal.test(LJR_SRL ~ Age_Class, data = PP) 

wilcox.test(SRL_TL ~ Maturity, data = PP) #SIGNIFICANT
t.test(SRL_TL ~ Maturity, data = PP) 
t.test(SRL_TRL ~ Maturity, data = PP) 
wilcox.test(LJR_SRL ~ Maturity, data = PP)

wilcox.test(SRL_TL ~ AgeC1, data = dfP)
wilcox.test(SRL_TL ~ AgeC2, data = dfP)
wilcox.test(SRL_TL ~ AgeC3, data = dfP)
wilcox.test(SRL_TL ~ AgeC4, data = dfP)
wilcox.test(SRL_TL ~ AgeC5, data = dfP)
wilcox.test(SRL_TL ~ AgeC6, data = dfP)
wilcox.test(SRL_TL ~ AgeC7, data = dfP)
wilcox.test(SRL_TL ~ AgeC8, data = dfP)
wilcox.test(SRL_TL ~ AgeC9, data = dfP)


t.test(SRL_TL ~ AgeC1, data = dfP)
t.test(SRL_TL ~ AgeC2, data = dfP)
t.test(SRL_TL ~ AgeC3, data = dfP)
t.test(SRL_TL ~ AgeC4, data = dfP)
t.test(SRL_TL ~ AgeC5, data = dfP)
t.test(SRL_TL ~ AgeC6, data = dfP)
t.test(SRL_TL ~ AgeC7, data = dfP)
t.test(SRL_TL ~ AgeC8, data = dfP)
t.test(SRL_TL ~ AgeC9, data = dfP)

PP %>%
  group_by(AgeC4) %>% #means Residuals by group SRL
  summarise(
    count = n(),
    mean = mean(SRL_TL, na.rm = TRUE),
    sd = sd(SRL_TL, na.rm = TRUE)
  )%>%
  ungroup()

#visualise SRL to TL dep on age class (not significant)
PP_SRL <- PP$SRL
PP_TL <- PP$TL_curve
cor(PP_SRL, PP_TL)
ggplot(PP, aes(x=SRL, y=TL_ImgJ, color=AgeC3)) + 
  geom_point()+
  geom_smooth(method=lm)

#Boxplot Age Ratio
ggplot(PP, aes(x=Age_Class, y=LJR_SRL, color=Age_Class)) + 
  geom_boxplot() +
  #coord_flip() +
  labs(title = "LJR:SRL FW")

ggplot(PP, aes(x=AgeC2, y=LJR_SRL, color=AgeC2)) + 
  geom_boxplot() +
  #coord_flip() +
  labs(title = "LJR:SRL FW")

ggplot(PP, aes(x=AgeC3, y=LJR_SRL, color=AgeC3)) + 
  geom_boxplot() +
  #coord_flip() +
  labs(title = "LJR:SRL FW")

ggplot(PP, aes(x=Age_Class, y=SRL_TL, color=Age_Class)) + 
  geom_boxplot() +
  #coord_flip() +
  labs(title = "SRL_TL FW")

ggplot(PP, aes(x=AgeC2, y=SRL_TL, color=AgeC2)) + 
  geom_boxplot() +
  #coord_flip() +
  labs(title = "SRL_TL FW")

ggplot(PP, aes(x=AgeC3, y=SRL_TL, color=AgeC3)) + 
  geom_boxplot() +
  #coord_flip() +
  labs(title = "SRL_TL FW")

##Isometry----

model_P <- lm(log(SRL) ~ log(TL_curve), data = PP)
summary(model_P)

cor.test(~ log(SRL) + log(TL_curve), data = PP, 
         method = "spearman", continuity = FALSE, exact = FALSE)

cor.test(~ log(SRL) + log(TL_curve), data = PP, 
         method = "kendall", continuity = FALSE, exact = FALSE)

##TRL vs SRL----

#MEANS

Pres %>%
  group_by(Publication, Size_Class) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE))%>%
  ungroup()

Pres %>%
  group_by(Publication, Size_Class) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResT, na.rm = TRUE),
    sd = sd(StNetResT, na.rm = TRUE))%>%
  ungroup()

#t tests

#overall

dfPR %>%  #ttest - calc vs true SRL
  do(tidy(t.test(.$StResS,
                 .$StResT,
                 mu = 0,
                 alt = "two.sided",
                 paired = TRUE,
                 conf.level = 0.99)))

dfPR %>%  #ttest - calc vs true SRL
  do(tidy(wilcox.test(.$TLCalc_SRL,
                      .$TLCalc_TRL,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      conf.level = 0.99)))
#size/maturity

dfPR %>%  #ttest - calc vs true SRL
  group_by(Size_Class) %>%
  do(tidy(wilcox.test(.$NetRes_S,
                      .$NetRes_T,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      conf.level = 0.99)))
ungroup()

dfPR %>%  #ttest - calc vs true SRL
  group_by(Size_Class) %>%
  do(tidy(t.test(.$StResS,
                 .$StResT,
                 mu = 0,
                 alt = "less",
                 paired = TRUE,
                 conf.level = 0.99)))
ungroup()

dfPR %>%  #ttest - calc vs true SRL
  group_by(Maturity) %>%
  do(tidy(wilcox.test(.$NetRes_S,
                      .$NetRes_T,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      conf.level = 0.99)))
ungroup()

dfPR %>%  #ttest - calc vs true SRL
  group_by(Maturity) %>%
  do(tidy(t.test(.$StResS,
                 .$StResT,
                 mu = 0,
                 alt = "less",
                 paired = TRUE,
                 conf.level = 0.99)))
ungroup()

#publication

dfPR %>%  #ttest - calc vs true SRL
  group_by(Publication) %>%
  do(tidy(wilcox.test(.$NetRes_S,
                      .$NetRes_T,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      exact = FALSE,
                      conf.level = 0.99))) %>%
  ungroup()

dfPR %>%  #ttest - calc vs true SRL
  group_by(Publication) %>%
  do(tidy(t.test(.$StNetResS,
                 .$StNetResT,
                 mu = 0,
                 alt = "two.sided",
                 paired = TRUE,
                 exact = FALSE,
                 conf.level = 0.99))) %>%
  ungroup()

dfPR %>%  #ttest - calc vs true SRL
  group_by(Publication) %>%
  do(tidy(wilcox.test(.$TLCalc_SRL,
                      .$TLCalc_TRL,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      exact = FALSE,
                      conf.level = 0.99))) %>%
  ungroup()

#interaction

dfPR %>%  #ttest - calc vs true SRL
  group_by(Publication, Size_Class) %>%
  do(tidy(t.test(.$StNetResS,
                 .$StNetResT,
                 mu = 0,
                 alt = "less",
                 paired = TRUE,
                 exact = FALSE,
                 conf.level = 0.99))) %>%
  ungroup()

dfPR %>%  #ttest - calc vs true SRL
  group_by(Publication, Maturity) %>%
  do(tidy(t.test(.$StNetResS,
                 .$StNetResT,
                 mu = 0,
                 alt = "greater",
                 paired = TRUE,
                 exact = FALSE,
                 conf.level = 0.99))) %>%
  ungroup()

#Now visualise this! 

PSNRP1 <- data_summary(Pres, varname = "StNetResS", 
                      groupnames=c("Publication", "Maturity", "ModelT"))

PSNRP1$Maturity=as.factor(PSNRP1$Maturity)
head(PSNRP1)
####Barplot----
StBarPR1 <- ggplot(PSNRP1[order(PSNRP1$Maturity,decreasing=T),],
                  aes(x=reorder(Publication, +StNetResS),
                      y=StNetResS, 
                      fill=factor(Maturity, levels = c("Mature", "Immature")),
                      pattern = ModelT)) + 
  geom_bar(stat="identity", 
           position="stack") +
  geom_bar_pattern(stat="identity",
                   position = "stack",
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.09,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(values = c(Lin = "stripe", Pow = "none"),
                       labels = c('Linear', 'Power')) +
  geom_text(aes(label = paste(round(StNetResS,1),"±", round(sd,1))),
            position = position_stack(), hjust = 1.2, size = 3, fontface = 'bold', color = "white") +
  labs(title = "SRL Input", x = "Publication", y = "Standardised Net Residuals Produced", pattern = "Model Type") +
  theme_minimal() +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))+
  scale_x_discrete(label = c("Thorburn et al. (2008) - Female", 
                             "Whitty et al. (2014) - Ratio",
                             "Peverell (2010)",
                             expression(paste(italic("*Morgan et al. (2011)"))),
                             expression(paste(bold("Faria et al. (2007) - Linear"))),
                             "Whitty et al. (2014) - Power",
                             "Thorburn et al. (2008) - Male",
                             expression(paste(bold("Faria et al. (2007) - Power")))
  )) +
  theme(plot.title = element_text(face = "italic"), 
        legend.position = c(0.82, 0.25), 
        legend.background = element_rect(fill="white")) +
  coord_flip() +
  ylim(0, 47) +
  scale_fill_manual(name = "Maturity",
                    labels=c('Mature', 'Immature'), 
                    values = c("grey40", "grey70"))
StBarPR1

PSNRT1 <- data_summary(Pres, varname = "StNetResT", 
                      groupnames=c("Publication", "Maturity", "ModelT"))

PSNRT1$Maturity=as.factor(PSNRT1$Maturity)
head(PSNRT1)

StBarPRT2 <- ggplot(PSNRT1[order(PSNRT1$Maturity,decreasing=T),],
                   aes(x=factor(Publication, level = c("Thorburn F",
                                                       "Whitty Linear",
                                                       "Peverell",
                                                       "Morgan",
                                                       "Faria Linear",
                                                       "Whitty Power",
                                                       "Thorburn M",
                                                       "Faria Power")),
                       y=StNetResT, 
                       fill=factor(Maturity, levels = c("Mature", "Immature")),
                       pattern = ModelT)) + 
  geom_bar(stat="identity", 
           position="stack") +
  geom_bar_pattern(stat="identity",
                   position = "stack",
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.09,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(values = c(Lin = "stripe", Pow = "none"),
                       labels = c('Linear', 'Power')) +
  geom_text(aes(label = paste(round(StNetResT,1),"±", round(sd,1))),
            position = position_stack(), hjust = 1.2, size = 3, fontface = 'bold', color = "white") +
  labs(title = "TRL Input", x = "Publication", y = "Standardised Net Residuals Produced", pattern = "Model Type") +
  theme_minimal() +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))+
  theme(plot.title = element_text(face = "italic"), 
        legend.position =c(0.82, 0.4),
        legend.background = element_rect(fill="white")) +
  coord_flip() +
  ylim(0, 47) +
  scale_x_discrete(label = c("Thorburn et al. (2008) - Female", 
                             "Whitty et al. (2014) - Ratio",
                             "Peverell (2010)",
                             expression(paste(italic("*Morgan et al. (2011)"))),
                             expression(paste(bold("Faria et al. (2007) - Linear"))),
                             "Whitty et al. (2014) - Power",
                             "Thorburn et al. (2008) - Male",
                             expression(paste(bold("Faria et al. (2007) - Power")))
  )) +
  scale_fill_manual(name = "Maturity",
                    labels=c('Mature', 'Immature'), 
                    values = c("grey40", "grey70"))
StBarPRT2

cowplot::plot_grid(StBarPR1 + theme(axis.text.x = element_blank(),
                                   axis.title.x = element_blank(),
                                   legend.position = "none"),
                   StBarPRT2,
                   labels = "AUTO", ncol = 1, align ="v",
                   rel_heights = c(.85,1))

ggsave(
  "PrissTvS.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 10,
  height = 6,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

##Residuals by Age----
sumtable(Pres, group = "Age_Class", group.long=T) #as Table

####Means----
Pres %>%
  group_by(Age_Class) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(Residuals_SRL, na.rm = TRUE),
    sd = sd(Residuals_SRL, na.rm = TRUE)
  )%>%
  ungroup()

Pres %>%
  group_by(Age_Class) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(NetRes_S, na.rm = TRUE),
    sd = sd(NetRes_S, na.rm = TRUE)
  )%>%
  ungroup()

Pres %>%
  group_by(Age_Class) %>% #means Residuals by group SRL
  summarise(
    count = n(),
    mean = mean(Residuals_TRL, na.rm = TRUE),
    sd = sd(Residuals_TRL, na.rm = TRUE)
  )%>%
  ungroup()

Pres %>%
  group_by(Age_Class) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(SSq_SRL, na.rm = TRUE),
    sd = sd(SSq_SRL, na.rm = TRUE)
  )%>%
  ungroup()

Pres %>%
  group_by(Age_Class) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(SSq_TRL, na.rm = TRUE),
    sd = sd(SSq_TRL, na.rm = TRUE)
  )%>%
  ungroup()

Pres %>% #wilcoxtest
  group_by(Age_Class) %>%
  do(tidy(wilcox.test(.$SSq_SRL,
                      .$SSq_TRL,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      conf.level = 0.99))) %>%
  ungroup()


wtPSRLAge <- Pres %>% #wilcoxtest
  group_by(Age_Class) %>%
  do(tidy(wilcox.test(.$TLCalc_SRL,
                      .$TLTrue,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      conf.level = 0.99))) %>%
  ungroup()
wtPSRLAge

Pres %>%
  group_by(Maturity) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(Residuals_SRL, na.rm = TRUE),
    sd = sd(Residuals_SRL, na.rm = TRUE)
  )%>%
  ungroup()

wtPSRLAge2 <- Pres %>% #wilcoxtest
  group_by(Maturity) %>%
  do(tidy(wilcox.test(.$TLCalc_SRL,
                      .$TLTrue,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      conf.level = 0.99))) %>%
  ungroup()
wtPSRLAge2

####ANOVA Age----
aovPAge <- aov(Residuals_SRL ~ Age_Class, Pres)
summary(aovPAge)

kruskal.test(Residuals_SRL ~ Age_Class, Pres)
kruskal.test(NetRes_S ~ Age_Class, Pres)
kruskal.test(SSq_SRL ~ Age_Class, Pres)

####Boxplot----
ggplot(Pres, aes(Age_Class, Residuals_SRL, color=Age_Class)) +
  geom_boxplot() +
  #coord_flip() +
  labs(title = "Projected Total Length of Pristis SRL ", 
       x= bquote(NULL),
       y= bquote("Residuals"))

ggplot(Pres, aes(Age_Class, Residuals_TRL, color=Age_Class)) +
  geom_boxplot() +
  #coord_flip() +
  labs(title = "Projected Total Length of Pristis TRL ", 
       x= bquote(NULL),
       y= bquote("Residuals"))

##Residuals by Pub----
sumtable(Pres, group = "Publication", group.long=T) #as Table

####Means Pub----
library(dplyr)

Pres %>%
  group_by(Publication) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(Residuals_SRL, na.rm = TRUE),
    sd = sd(Residuals_SRL, na.rm = TRUE)
  )%>%
  ungroup()

Pres %>%
  group_by(Publication) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(NetRes_S, na.rm = TRUE),
    sd = sd(NetRes_S, na.rm = TRUE)
  )%>%
  ungroup()

Pres %>%
  group_by(Publication) %>% #means Residuals by group TRL
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)
  ) %>%
  ungroup()

####t-test----
wtPSRL <- Pres %>%  #ttest - calc vs true SRL
  group_by(Publication) %>%
  do(tidy(wilcox.test(.$TLCalc_SRL,
                      .$TLTrue,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      conf.level = 0.99))) %>%
  ungroup()
wtPSRL


tPSRL <- Pres %>%  #ttest - calc vs true SRL
  group_by(Publication) %>%
  do(tidy(t.test(.$TLCalc_SRL,
                 .$TLTrue,
                 mu = 0,
                 alt = "two.sided",
                 paired = TRUE,
                 conf.level = 0.99))) %>%
  ungroup()
tPSRL

logtPSRL <- Pres %>%  #ttest - calc vs true SRL
  group_by(Publication) %>%
  do(tidy(t.test(.$sinlogS,
                 mu = 0,
                 alt = "two.sided",
                 conf.level = 0.99))) %>%
  ungroup()
logtPSRL

ThorFM <- data.frame(
  Pub = c(rep("ResidThorF", 36), rep("ResidThorM", 36)),
  Resid = c(dfPT$ResidThorF, dfPT$ResidThorM)
)
ThorFM

wilcox.test(Resid ~ Pub, ThorFM, paired = TRUE)

pwPTRL <- Pres %>%  #ttest - calc vs true TRL
  group_by(Publication) %>%
  do(tidy(t.test(.$TLCalc_TRL,
                 .$TLTrue,
                 mu = 0,
                 alt = "two.sided",
                 paired = TRUE,
                 conf.level = 0.99))) %>%
  ungroup()
pwPTRL

Pres %>%
  group_by(ModelT) %>% #means Residuals by group TRL
  dplyr::summarise(
    count = n(),
    mean = mean(NetRes_S, na.rm = TRUE),
    sd = sd(NetRes_S, na.rm = TRUE)
  ) %>%
  ungroup()

Pres %>%
  group_by(ModelT) %>% #means Residuals by group TRL
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)
  ) %>%
  ungroup()

Pres %>%
  group_by(Maturity, ModelT) %>% #means Residuals by group TRL
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)
  ) %>%
  ungroup()

####ANOVA Pub on Resid----
aovPPUB <- aov(logResS ~ Publication + Age_Class + Publication:Age_Class, Pres)
summary(aovPPUB)

kruskal.test(NetRes_S ~ Publication, Pres)
kruskal.test(StNetResS ~ Publication, Pres) #notsig

wilcox.test(NetRes_S ~ ModelT, Pres)
t.test(logResS ~ ModelT, Pres)
t.test(StNetResS ~ ModelT, Pres)

####Pristis Resid Corr----
sapply(
  split(data.frame(Pres$TLTrue, Pres$TLCalc_SRL), Pres$Publication), 
  function(x) cor(x[[1]],x[[2]])
)

##Association Age-Pub----
####Means----
PAP1<- Pres %>%
  group_by(Publication, Age_Class) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(NetRes_S, na.rm = TRUE),
    sd = sd(NetRes_S, na.rm = TRUE)) %>%
  ungroup()
print(PAP1, n = Inf)

PAP2<- Pres %>%
  group_by(Publication, Age_Class) %>% #means Residuals by group Pub
  summarise(
    count = n(),
    mean = mean(StResS, na.rm = TRUE),
    sd = sd(StResS, na.rm = TRUE)) %>%
  ungroup()
print(PAP2, n = Inf)

PAP3<- Pres %>%
  group_by(Publication, Age_Class) %>% #means Residuals by group Pub
  summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)) %>%
  ungroup()
print(PAP3, n = Inf)

Pres %>%
  group_by(Publication, Maturity) %>% #means Residuals by group Pub
  summarise(
    count = n(),
    mean = mean(NetRes_S, na.rm = TRUE),
    sd = sd(NetRes_S, na.rm = TRUE)) %>%
  ungroup()

Pres %>%
  group_by(Publication, Maturity) %>% #means Residuals by group Pub
  summarise(
    count = n(),
    mean = mean(StResS, na.rm = TRUE),
    sd = sd(StResS, na.rm = TRUE)) %>%
  ungroup()

Pres %>%
  group_by(Publication, Maturity) %>% #means Residuals by group Pub
  summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)) %>%
  ungroup()

Pres %>%
  group_by(ModelT, Maturity) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)) %>%
  ungroup()

Pres %>%
  group_by(Publication, Size_Class) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)) %>%
  ungroup()

Pres %>%
  group_by(Publication, Size_Class) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(NetRes_S, na.rm = TRUE),
    sd = sd(NetRes_S, na.rm = TRUE)) %>%
  ungroup()

####t-Test split----
t.test(formula = SNStP ~ Size_Class, data = dfPT) #significant
t.test(formula = SNStFL ~ Size_Class, data = dfPT) #notsignificant
t.test(formula = SNStFP ~ Size_Class, data = dfPT) #notsignificant
t.test(formula = SNStWC ~ Size_Class, data = dfPT) #notsignificant
t.test(formula = SNStWL ~ Size_Class, data = dfPT) #notsignificant
t.test(formula = SNStM ~ Size_Class, data = dfPT) #notsignificant
t.test(formula = SNStTF ~ Size_Class, data = dfPT) #notsignificant
t.test(formula = SNStTM ~ Size_Class, data = dfPT) #notsignificant

t.test(formula = SNStP ~ Maturity, data = dfPT) #notsignificant
t.test(formula = SNStFL ~ Maturity, data = dfPT) #notsignificant
t.test(formula = SNStFP ~ Maturity, data = dfPT) #significant
t.test(formula = SNStWC ~ Maturity, data = dfPT) #notsignificant
t.test(formula = SNStWL ~ Maturity, data = dfPT) #notsignificant
t.test(formula = SNStM ~ Maturity, data = dfPT) #notsignificant
t.test(formula = SNStTF ~ Maturity, data = dfPT) #notsignificant
t.test(formula = SNStTM ~ Maturity, data = dfPT) #significant

wilcox.test(formula = NetP ~ Size_Class, data = dfPT) #notsignificant
wilcox.test(formula = NetFL ~ Size_Class, data = dfPT) #significant
wilcox.test(formula = NetFP ~ Size_Class, data = dfPT) #significant
wilcox.test(formula = NetWC ~ Size_Class, data = dfPT) #significant
wilcox.test(formula = NetWL ~ Size_Class, data = dfPT) #significant
wilcox.test(formula = NetM ~ Size_Class, data = dfPT) #notsignificant
wilcox.test(formula = NetTF ~ Size_Class, data = dfPT) #significant
wilcox.test(formula = NetTM ~ Size_Class, data = dfPT) #significant

wilcox.test(formula = NetP ~ Maturity, data = dfPT)
wilcox.test(formula = NetFL ~ Maturity, data = dfPT)
wilcox.test(formula = NetFP ~ Maturity, data = dfPT)
wilcox.test(formula = NetWC ~ Maturity, data = dfPT)
wilcox.test(formula = NetWL ~ Maturity, data = dfPT)
wilcox.test(formula = NetM ~ Maturity, data = dfPT) #notsignificant
wilcox.test(formula = NetTF ~ Maturity, data = dfPT)
wilcox.test(formula = NetTM ~ Maturity, data = dfPT)

t.test(formula = LogP ~ Size_Class, data = dfPT) #notsignificant
t.test(formula = LogFL ~ Size_Class, data = dfPT) #significant
t.test(formula = LogFP ~ Size_Class, data = dfPT) #significant
t.test(formula = LogM ~ Size_Class, data = dfPT) #notsignificant
t.test(formula = LogTM ~ Size_Class, data = dfPT) #significant
t.test(formula = LogTF ~ Size_Class, data = dfPT) #significant
t.test(formula = LogWC ~ Size_Class, data = dfPT) #significant
t.test(formula = LogWL ~ Size_Class, data = dfPT) #significant

t.test(formula = LogP ~ Maturity, data = dfPT) #significant
t.test(formula = LogFL ~ Maturity, data = dfPT) #significant
t.test(formula = LogFP ~ Maturity, data = dfPT) #significant
t.test(formula = LogM ~ Maturity, data = dfPT) #notsignificant
t.test(formula = LogTM ~ Maturity, data = dfPT) #significant
t.test(formula = LogTF ~ Maturity, data = dfPT) #significant
t.test(formula = LogWC ~ Maturity, data = dfPT) #significant
t.test(formula = LogWL ~ Maturity, data = dfPT) #significant

####ANOVA Split by Pubs----

kruskal.test(NetP ~ Age_Class, data = Ptest) #1Way
kruskal.test(NetFL ~ Age_Class, data = Ptest) #1Way
kruskal.test(NetFP ~ Age_Class, data = Ptest) #1Way
kruskal.test(NetWL ~ Age_Class, data = Ptest) #1Way
kruskal.test(NetWC ~ Age_Class, data = Ptest) #1Way
kruskal.test(NetM ~ Age_Class, data = Ptest) #1Way
kruskal.test(NetTF ~ Age_Class, data = Ptest) #1Way
kruskal.test(NetTM ~ Age_Class, data = Ptest) #1Way

summary(aov(SNStP ~ Age_Class, data = Ptest)) #1Way
summary(aov(SNStFL ~ Age_Class, data = Ptest)) #1Way
summary(aov(SNStFP ~ Age_Class, data = Ptest)) #1Way
summary(aov(SNStWL ~ Age_Class, data = Ptest)) #1Way
summary(aov(SNStWC ~ Age_Class, data = Ptest)) #1Way
summary(aov(SNStM ~ Age_Class, data = Ptest)) #1Way
summary(aov(SNStTF ~ Age_Class, data = Ptest)) #1Way
summary(aov(SNStTM ~ Age_Class, data = Ptest)) #1Way

####State----
modelP1 = lm(SRL_TL ~ State + Maturity + State:Maturity, data = PP)
Anova(modelP1, type = "II")

modelP2 = lm(SRL_TL ~ State + Age_Class + State:Age_Class, data = PP)
Anova(modelP2, type = "II")

####BoxNDot Age-Pub----

ggplot(Pres, aes(Age_Class, Residuals_SRL, color=Publication)) +
  geom_boxplot() +
  #coord_flip() +
  labs(title = "Projected Total Length of Sawfishes ", 
       x= bquote(NULL),
       y= bquote("Residuals"))

ggplot(Pres, mapping=aes(x=TLTrue, y=Residuals_SRL, color=Publication)) +
  geom_point() +
  geom_hline(yintercept=0,linetype="dashed") +
  stat_ellipse(aes(x=TLTrue, y=Residuals_SRL, color=Publication),type = "norm") +
  scale_fill_manual(values = c("blue4","gold2", "red3", "yellow", "darkgreen", "grey", "light grey")) +
  labs(title = "Clavata Total Length Unused",
       x = bquote("Total Length"),
       y = bquote("Residuals"),
       fill = "Publication")

##Gradient----

modPP <- lm(SStP ~ TLCorr, data = Ptest)
modPFL <- lm(SStFL ~ TLCorr, data = Ptest)
modPFP <- lm(SStFP~ TLCorr, data = Ptest)
modPM <- lm(SStM ~ TLCorr, data = Ptest)
modPTF <- lm(SStTF~ TLCorr, data = Ptest)
modPTM <- lm(SStTM~ TLCorr, data = Ptest)
modPWL <- lm(SStWL~ TLCorr, data = Ptest)
modPWP<- lm(SStWC~ TLCorr, data = Ptest)

modelsPPub <- list(modPP, modPFL, modPFP, modPM, modPTF, modPTM, modPWL, modPWP)
modCnamP <- c('P', 'FL', 'FP', 'M', 'TF', 'TM', 'WL', 'WP')

summary(modPP)
summary(modPFL)
summary(modPFP)
summary(modPM)
summary(modPTF)
summary(modPTM)
summary(modPWL)
summary(modPWP)

##ToPrint----

####BarPlot Age-Class----
PRP <- data_summary(Pres, varname = "Residuals_SRL", 
                    groupnames=c("Publication", "Age_Class"))

PRP$Age_Class <- factor(PRP$Age_Class, levels = c("adult", "sub-adult", "juvenile", "YOY"))


PPwrap <- c("Whitty et al. (2014) Ratio",
            "Thorburn et al. (2007) - Female",
            "Peverell (2010)", 
            "Morgan et al. (2011)", 
            "Faria (2007) Linear",
            "Whitty et al. (2014) Power", 
            "Thorburn et al. (2007) - Male", 
            "Faria (2007) Power")

wrap_labelsP <- function(x) {
  str_wrap(PPwrap, width = 15)
}


BarPR1 <- ggplot(PRP, aes(x=factor(Publication, level = c("Whitty Linear",
                                                         "Thorburn F",
                                                         "Peverell",
                                                         "Morgan",
                                                         "Faria Linear",
                                                         "Whitty Power",
                                                         "Thorburn M",
                                                         "Faria Power")),
                         y=Residuals_SRL, 
                         fill=factor(Age_Class, level = c('YOY',
                                                          'juvenile',
                                                          'sub-adult',
                                                          'adult')))) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Residuals_SRL-sd, ymax=Residuals_SRL+sd), width=.2,
                position=position_dodge(.9)) +
  labs(title = "P. pristis", x = "Model", y = "Residuals (mm)") +
  theme(plot.title = element_text(face = "italic"),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        axis.title = element_text(size = 12)) + 
  scale_x_discrete(labels = c('WR', 'TF', 'P', 'M', 'L', 'W', 'TM', 'FP'))  +
  scale_fill_manual(name = "Size Class",
                    labels = c('YOY', 'Juvenile', 'Sub-adult', 'Adult'), 
                    values = c("#5ab4ac", "#f5f5f5", "56B4E9", "#d8b365")) 
print(BarPR1)

# Finished bar plot

####Stacked Bar----

PSNRP <- data_summary(Pres, varname = "StNetResS", 
                      groupnames=c("Publication", "Maturity", "ModelT"))

PSNRP$Maturity=as.factor(PSNRP$Maturity)
head(PSNRP)

install.packages("remotes")
remotes::install_github("coolbutuseless/ggpattern")
library(ggpattern)

StBarPR <- ggplot(PSNRP[order(PSNRP$Maturity,decreasing=T),],
                  aes(x=reorder(Publication, +StNetResS),
                             y=StNetResS, 
                             fill=factor(Maturity, levels = c("Mature", "Immature")),
                             pattern = ModelT)) + 
  geom_bar(stat="identity", 
           position="stack") +
  geom_bar_pattern(stat="identity",
                   position = "stack",
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.09,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(values = c(Lin = "stripe", Pow = "none")) +
  scale_x_discrete(label = c("Thorburn et al. (2007) - Female",
                             "Whitty et al. (2014) - Ratio",
                             "Peverell (2010)",
                             "Morgan et al. (2011)",
                             "Faria et al. (2007) - Linear",
                             "Whitty et al. (2014) - Power",
                             "Thorburn et al. (2007) - Male",
                             "Faria et al. (2007) - Power"
                             )) +
  geom_text(aes(label = paste(round(StNetResS,1),"±", round(sd,1))),
            position = position_stack(), hjust = 1.15, size = 3, color = "white") +
  labs(x = "Publication", y = "Standardised Net Residuals", pattern = "Model Type") +
  theme_minimal() +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))+
  theme(plot.title = element_text(face = "italic"), legend.position = c(0.87, 0.25), 
        legend.background = element_rect(fill="white")) +
  coord_flip() +
  ylim(0, 45) +
  scale_fill_manual(name = "Maturity",
                    labels=c('Mature', 'Immature'), 
                    values = c("grey40", "grey70"))
StBarPR

ggsave(
  "PristisBar.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 12.5,
  height = 5,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

####Dotplot----

PPDP <- ggplot(Pres, mapping=aes(x=TLTrue, 
                                 y=StResS, 
                                 color=factor(Publication, level = c("Morgan",
                                                                     "Thorburn F",
                                                                     "Whitty Linear",
                                                                     "Faria Linear",
                                                                     "Whitty Power",
                                                                     "Peverell",
                                                                     "Thorburn M",
                                                                     "Faria Power")))) +
  coord_cartesian(xlim =c(700, 6350)) +
  geom_point(size = 0.75) +
  geom_hline(yintercept = c(-5, 0, 5),
             linetype = c('dotted', "longdash", 'dotted'),
             color = c('grey40', 'black', 'grey40')) +
  annotate("rect", xmin = -Inf, xmax = Inf, 
           ymin = -5.2, ymax= 5.2, alpha = 0.1, fill = 'grey40') +
  scale_color_manual(labels=c('Morgan et al. (2011)',
                              'Thorburn et al. (2007) - Female',
                              'Whitty et al. (2014) Ratio',
                              'Faria (2007) Linear', 
                              'Whitty et al. (2014) Power',
                              'Peverell (2010)',
                              'Thorburn et al. (2007) - Male',
                              'Faria (2007) Power'),
                     values = c('red',
                                'magenta',
                                'darkblue',
                                'orange',
                                'lightblue', 
                                rgb(0.2,0.4,0.1,0.7),
                                'purple',
                                'yellow')) +
  stat_smooth(aes(x=TLTrue, y=StResS, color=Publication), 
              method = lm, se = FALSE) +
  geom_vline(xintercept = c(760, 1200, 2700, 3000, 6400),
             linetype = c('solid', 'solid', 'dotdash', 'solid', 'solid')) +
  # annotate(geom = "text",
  #          label = c('Birth Size', 'One Year', ' ', 'Maturity', 'Max Size'),
  #          x = c(800, 1240, 2740, 3040, 6440),
  #          y = c(45, 45, 45, 45, 45),
  #          size = c(3.5,3.5,3.5,3.5,3.5),
  #          angle = 90,
  #          vjust = -1) +
  geom_segment(x = 760, y = 54.5, xend = 1200, yend = 54.5,
               colour = "#5ab4ac",
               size = 3) +
  geom_segment(x = 1200, y = 54.5, xend = 2700, yend = 54.5,
               colour = "#f5f5f5",
               size = 3) +
  geom_segment(x = 2700, y = 54.5, xend = 3000, yend = 54.5,
               colour = "lightgrey",
               size = 3) +
  geom_segment(x = 3000, y = 54.5, xend = 6400, yend = 54.5,
               colour = "#d8b365",
               size = 3) +
  # annotate(geom = "label",
  #          label = c('YOY', 'Juvenile', 'Sub-Adult', 'Adult'),
  #          x = c((760+1200)/2, (1200+2700)/2, (2700+3000)/2, (3000+6400)/2),
  #          y = c(52, 52, 52, 52),
  #          size = c(4,4,4,4),
  #          fill = c("#5ab4ac", "#f5f5f5", "lightgrey", "#d8b365"),
  #          colour = c("white", "black", "black", "white"),
  #          angle = 0,
  #          vjust = 1) +
  theme(plot.title = element_text(face = "italic"),
        legend.key = element_rect(fill = "white"),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "bottom") +
  labs(title = "Pristis pristis",
       x = bquote("Total Length (mm)"),
       y = bquote("Percent Error (%)"),
       color = "Model")
PPDP

####LetsPrint----

####Pub Section Pic----
#Bar + Box + Dot
plotsPP <- align_plots(BarPR + theme(plot.title = element_blank(),
                                   legend.position = "none"), 
                     PPDP + theme(plot.title = element_blank()), 
                     align = 'v', axis = 'l')
topP <- plot_grid(plotsPP[[1]], StBarPR + theme(plot.title = element_blank()),
                  labels = c('A', 'B'))

cowplot::plot_grid(topP, plotsPP[[2]],
                   labels = c('','C'), nrow = 2, rel_heights = c(1.25,2))

#Bar Box Only
cowplot::plot_grid(BarPR + theme(plot.title = element_blank(),
                                 legend.position = "none"),
                   PPDP + theme(plot.title = element_blank()),
                   labels = "auto", ncol = 1, rel_heights = c(1,2), align ="v", axis = 'l')


ggsave(
  "PristisData.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 12,
  height = 9,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)


##Appx: Residual Plot----
####Big Boi----
library(cowplot)
Pa <- ggplot(dfPT, aes(x = SRL, y = TLCorr)) + #Pev
  geom_smooth(aes(x = SRL, y = Pev_SRL), method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = SRL, yend = Pev_SRL), alpha = .2) +
  ylim(0, 9000) +
  geom_point(aes(color = ResidP)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  geom_point(aes(y = TLCorr), shape = 1) +
  labs(x= "SRL", y = "Total Length", 
       title= "Peverell, 2010", color ="Residuals") +
  theme_bw()
Pb <- ggplot(dfPT, aes(x = SRL, y = TLCorr)) + #FariaL
  geom_smooth(aes(x = SRL, y = FariaL_SRL), method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = SRL, yend = FariaL_SRL), alpha = .2) +
  ylim(0, 9000) +
  geom_point(aes(color = ResidFL)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  geom_point(aes(y = TLCorr), shape = 1) +
  labs(x= "SRL", y = "Total Length", 
       title= "Faria, 2007 - Linear", color ="Residuals") +
  theme_bw()
Pc <- ggplot(dfPT, aes(x = SRL, y = TLCorr)) + #FariaE
  stat_smooth(aes(x = SRL, y = FariaE_SRL), method = "loess", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = SRL, yend = FariaE_SRL), alpha = .2) +
  ylim(0, 9000) +
  geom_point(aes(color = ResidFE)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  geom_point(aes(y = TLCorr), shape = 1) +
  labs(x= "SRL", y = "Total Length", 
       title= "Faria, 2007 - Power", color ="Residuals") +
  theme_bw()
Pd <- ggplot(dfPT, aes(x = SRL, y = TLCorr)) + #WhittyP
  stat_smooth(aes(x = SRL, y = WhittyP_SRL), method = "loess", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = SRL, yend = WhittyP_SRL), alpha = .2) +
  ylim(0, 9000) +
  geom_point(aes(color = ResidWC)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  geom_point(aes(y = TLCorr), shape = 1) +
  labs(x= "SRL", y = "Total Length", 
       title= "Whitty et al. 2014 - Power", color ="Residuals") +
  theme_bw()
Pe <- ggplot(dfPT, aes(x = SRL, y = TLCorr)) + #WhittyL
  geom_smooth(aes(x = SRL, y = WhittyL_SRL), method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = SRL, yend = WhittyL_SRL), alpha = .2) +
  ylim(0, 9000) +
  geom_point(aes(color = ResidWL)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  geom_point(aes(y = TLCorr), shape = 1) +
  labs(x= "SRL", y = "Total Length", 
       title= "Whitty et al. 2014 - Ratio", color ="Residuals") +
  theme_bw()
Pf <- ggplot(dfPT, aes(x = SRL, y = TLCorr)) + #Morgan
  geom_smooth(aes(x = SRL, y = Morg_SRL), method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = SRL, yend = Morg_SRL), alpha = .2) +
  ylim(0, 9000) +
  geom_point(aes(color = ResidM)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  geom_point(aes(y = TLCorr), shape = 1) +
  labs(x= "SRL", y = "Total Length", 
       title= "Morgan et al. 2011", color ="Residuals") +
  theme_bw()
Pg <- ggplot(dfPT, aes(x = SRL, y = TLCorr)) + #ThorM
  geom_smooth(aes(x = SRL, y = ThorM_SRL), method = "loess", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = SRL, yend = ThorM_SRL), alpha = .2) +
  ylim(0, 9000) +
  geom_point(aes(color = ResidThorM)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  geom_point(aes(y = TLCorr), shape = 1) +
  labs(x= "SRL", y = "Total Length", 
       title= "Thorburn et al. 2007 - Male", color ="Residuals") +
  theme_bw()
Ph <- ggplot(dfPT, aes(x = SRL, y = TLCorr)) + #ThorF
  geom_smooth(aes(x = SRL, y = ThorF_SRL), method = "loess", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = SRL, yend = ThorF_SRL), alpha = .2) +
  ylim(0, 9000) +
  geom_point(aes(color = ResidThorF)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  geom_point(aes(y = TLCorr), shape = 1) +
  labs(x= "SRL", y = "Total Length", 
       title= "Thorburn et al. 2007 - Female", color ="Residuals") +
  theme_bw()

PResGALL <- cowplot::plot_grid(Pb, 
                               Pf + theme(axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank(),
                                          axis.title.y = element_blank() ),
                               Pa + theme(axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank(),
                                          axis.title.y = element_blank() ),
                               Pe + theme(axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank(),
                                          axis.title.y = element_blank() ),
                               Pc,
                               Ph + theme(axis.text.y = element_blank(),
                                         axis.ticks.y = element_blank(),
                                         axis.title.y = element_blank() ), 
                               Pg + theme(axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank(),
                                          axis.title.y = element_blank() ),
                               Pd + theme(axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank(),
                                          axis.title.y = element_blank() ),
                               labels = "AUTO", ncol = 4, align ="v")
PResGALL

ggsave(
  "PristisRes.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 12.5,
  height = 5,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

###ModelType----

Pres$ModelT = factor(Pres$ModelT, levels = unique (Pres$ModelT))
Pres$Maturity = factor(Pres$Maturity, levels = unique (Pres$Maturity))

if(!require(car)){install.packages("car")}
if(!require(psych)){install.packages("psych")}
if(!require(multcomp)){install.packages("multcomp")}
if(!require(emmeans)){install.packages("emmeans")}
if(!require(FSA)){install.packages("FSA")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(phia)){install.packages("phia")}

library(psych)

summary(Pres)

interaction.plot(x.factor     = Pres$ModelT,
                 trace.factor = Pres$Maturity,
                 response     = Pres$StNetResS,
                 fun = mean,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")

modelPT = lm(StNetResS ~ ModelT + Maturity + ModelT:Maturity, data = Pres)
Anova(modelPT, type = "II")

#ZijsronTest----
###Normality----
####Ratios----
library(car)
library(ggpubr)
library(rstatix)

ggqqplot(PZ$LJR_SRL)
ggdensity(PZ$LJR_SRL,
          main = "Density plot of LJR/SRL",
          xlab = "LJR/SRL")
shapiro.test(PZ$LJR_SRL) #normal

ggqqplot(PZ$SRL_TL)
ggdensity(PZ$SRL_TL,
          main = "Density plot of SRL/TL",
          xlab = "SRL_TL")
shapiro.test(PZ$SRL_TL) #normal

ggqqplot(PZ$SRL_TRL)
ggdensity(PZ$SRL_TRL,
          main = "Density plot of SRL/TRL",
          xlab = "SRL_TL")
shapiro.test(PZ$SRL_TRL) #normal

ggqqplot(PZ$TL_curve)
ggdensity(PZ$TL_curve,
          main = "Density plot of TL",
          xlab = "TL")
shapiro.test(PZ$TL_curve) # sorta normal

dfZ %>%
  ggplot(aes(x=LJR_SRL, fill=Age_Class))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="LJR/SRL Ratio")

dfZ %>%
  ggplot(aes(x=LJR_SRL, fill=Maturity))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="LJR/SRL Ratio")

#visualise diff in ratio by size

dfZ %>%
  ggplot(aes(x=LJR_SRL, fill=AgeC3))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="LJR/SRL Ratio")

#SRL/TL

dfZ %>%
  ggplot(aes(x=SRL_TL, fill=Age_Class))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="SRL_TL Ratio")

dfZ %>%
  ggplot(aes(x=SRL_TL, fill=Maturity))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="SRL_TL Ratio")

dfZ %>%
  ggplot(aes(x=TL_ImgJ, fill=Age_Class))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="TL")

dfZ %>%
  ggplot(aes(x=TL_ImgJ, fill=Maturity))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="TL")

#SRL/TRL

dfZ %>%
  ggplot(aes(x=SRL_TRL, fill=Age_Class))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="SRL/TRL")

dfZ %>%
  ggplot(aes(x=SRL_TRL, fill=Maturity))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="SRL/TRL")

####Residuals----
ggqqplot(Zres$Residuals_SRL)
ggdensity(Zres$Residuals_SRL,
          main = "Density plot of Residuals",
          xlab = "Residuals - SRL")
shapiro.test(Zres$Residuals_SRL) #not normal

ggqqplot(Zres$NetRes_S)
ggdensity(Zres$NetRes_S,
          main = "Density plot of Residuals",
          xlab = "Residuals - SRL")
shapiro.test(Zres$NetRes_S) #not normal

ggqqplot(Zres$logNetRS)
ggdensity(Zres$logNetRS,
          main = "Density plot of Residuals",
          xlab = "Residuals - SRL")
shapiro.test(Zres$logNetRS) #almost normal

ggqqplot(Zres$StResS)
ggdensity(Zres$StResS,
          main = "Density plot of Residuals",
          xlab = "Residuals - SRL")
shapiro.test(Zres$StResS) #normal

dfZR %>%
  ggplot(aes(x=Residuals_SRL, fill=Publication))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Residuals - SRL")

dfZR %>%
  group_by(Publication) %>%
  shapiro_test(Residuals_SRL) %>%
  ungroup()

dfZR %>%
  ggplot(aes(x=NetRes_S, fill=Publication))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Net Res")

dfZR %>%
  group_by(Publication) %>%
  shapiro_test(NetRes_S) %>%
  ungroup()

dfZR %>%
  ggplot(aes(x=StResS, fill=Publication))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Residuals - SRL")

dfZR %>%
  group_by(Publication) %>%
  shapiro_test(StResS) %>%
  ungroup()

dfZR %>%
  ggplot(aes(x=logNetRS, fill=Publication))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Log Residuals - SRL")

dfZR %>%
  group_by(Publication) %>%
  shapiro_test(logNetRS) %>%
  ungroup()

#AgeClass

dfZR %>%
  ggplot(aes(x=NetRes_S, fill=Age_Class))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Net Residuals - SRL")

dfZR %>%
  group_by(Age_Class) %>%
  shapiro_test(NetRes_S) %>%
  ungroup()

dfZR %>%
  ggplot(aes(x=logNetRS, fill=Age_Class))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Log Residuals - SRL")

dfZR %>%
  group_by(Age_Class) %>%
  shapiro_test(logNetRS) %>%
  ungroup()

dfZR %>%
  ggplot(aes(x=StResS, fill=Age_Class))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Log Residuals - SRL")

dfZR %>%
  group_by(Age_Class) %>%
  shapiro_test(StResS) %>%
  ungroup()

###Variance----
####Ratios----
PZ$Age_Class <- as.factor(PZ$Age_Class)
leveneTest(LJR_SRL ~ Age_Class, data = PZ) #equal var
leveneTest(SRL_TL ~ Age_Class, data = PZ) #equal var
leveneTest(SRL_TRL ~ Age_Class, data = PZ) #equal var
leveneTest(TL_ImgJ ~ Age_Class, data = PZ) #equal var

####Residuals----
Zres$Publication <- as.factor(Zres$Publication)
leveneTest(TLCalc_SRL ~ Publication*Age_Class, data = Zres) #equal var
leveneTest(TLCalc_SRL ~ Publication, data = Zres) #equal var

leveneTest(Residuals_SRL ~ Publication*Age_Class, data = Zres) #unequal var
leveneTest(Residuals_SRL ~ Publication, data = Zres) #unequal var

leveneTest(TLCalc_TRL ~ Publication*Age_Class, data = Zres) #equal var
leveneTest(TLCalc_TRL ~ Publication, data = Pres) #equal var

##Test Ratios----
####Ratio Reliability----
#accuracy of measurements on ratios - not sig.

aovZ0 <- aov(LJR_SRL ~ State, data = PZ) #1Way
summary(aovZ0)
aovZ1 <- aov(SRL_TRL ~ State, data = PZ) #1Way
summary(aovZ1)
aovZ2 <- aov(SRL_TL ~ State, data = PZ) #1Way
summary(aovZ2)

t.test(formula = LJR_SRL~ Rost_Real, data = dfZ)
t.test(formula = LJR_SRL~ TL_Real, data = dfZ)
t.test(formula = LJR_SRL~ Multiple_Planes, data = dfZ)
t.test(formula = SRL_TL ~ Rost_Real, data = dfZ)
t.test(formula = SRL_TL ~ Multiple_Planes, data = dfZ)
t.test(formula = SRL_TRL ~ Rost_Real, data = dfZ)
t.test(formula = SRL_TRL ~ TL_Real, data = dfZ)
t.test(formula = SRL_TRL ~ Multiple_Planes, data = dfZ)

####Ratio by Age----
aovZ3 <- aov(SRL_TL ~ Age_Class, data = PZ) #NOT SIG
summary(aovZ3)
aovZ4 <- aov(SRL_TRL ~ Age_Class, data = PZ) #NOT SIG
summary(aovZ4)
aovZ5 <- aov(LJR_SRL ~ Age_Class, data = PZ) #NOT SIG
summary(aovZ5)

t.test(formula = LJR_SRL~ Maturity, data = dfZ)
t.test(formula = LJR_SRL~ AgeC1, data = dfZ)
t.test(formula = LJR_SRL~ AgeC2, data = dfZ)
t.test(formula = LJR_SRL~ AgeC3, data = dfZ)
t.test(formula = LJR_SRL~ AgeC4, data = dfZ)
t.test(formula = LJR_SRL~ AgeC5, data = dfZ)
t.test(formula = LJR_SRL~ AgeC6, data = dfZ)

t.test(formula = SRL_TL ~ Maturity, data = dfZ)
t.test(formula = SRL_TL ~ AgeC1, data = dfZ)
t.test(formula = SRL_TL ~ AgeC2, data = dfZ)
t.test(formula = SRL_TL ~ AgeC3, data = dfZ)
t.test(formula = SRL_TL ~ AgeC4, data = dfZ)
t.test(formula = SRL_TL ~ AgeC5, data = dfZ)
t.test(formula = SRL_TL ~ AgeC6, data = dfZ)

t.test(formula = SRL_TRL ~ Maturity, data = dfZ)
t.test(formula = SRL_TRL ~ AgeC1, data = dfZ)
t.test(formula = SRL_TRL ~ AgeC2, data = dfZ)
t.test(formula = SRL_TRL ~ AgeC3, data = dfZ)
t.test(formula = SRL_TRL ~ AgeC4, data = dfZ)
t.test(formula = SRL_TRL ~ AgeC5, data = dfZ)
t.test(formula = SRL_TRL ~ AgeC6, data = dfZ)

#visualise SRL to TL dep on age class (not significant)
##Isometry----

model_Z <- lm(log(SRL) ~ log(TL_curve), data = PZ)
summary(model_Z)

model_Z1 <- lm(log(LJR) ~ log(TL_curve), data = PZ)
summary(model_Z1)

####Boxplot Ratio----
ggplot(PZ, aes(x=Age_Class, y=LJR_SRL, color=Age_Class)) + 
  geom_boxplot() +
  #coord_flip() +
  labs(title = "LJR:SRL Zijsron")

ggplot(PZ, aes(x=AgeC2, y=LJR_SRL, color=AgeC2)) + 
  geom_boxplot() +
  #coord_flip() +
  labs(title = "LJR:SRL Zijsron")

ggplot(PZ, aes(x=AgeC3, y=LJR_SRL, color=AgeC3)) + 
  geom_boxplot() +
  #coord_flip() +
  labs(title = "LJR:SRL Zijsron")

ggplot(PZ, aes(x=Age_Class, y=SRL_TL, color=Age_Class)) + 
  geom_boxplot() +
  #coord_flip() +
  labs(title = "SRL_TL Zijsron")

ggplot(PZ, aes(x=AgeC2, y=SRL_TL, color=AgeC2)) + 
  geom_boxplot() +
  #coord_flip() +
  labs(title = "SRL_TL Zijsron")

ggplot(PZ, aes(x=AgeC3, y=SRL_TL, color=AgeC3)) + 
  geom_boxplot() +
  #coord_flip() +
  labs(title = "SRL_TL Zijsron")

##TRL vs SRL----

#MEANS

Zres %>%
  group_by(Publication, Size_Class) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE))%>%
  ungroup()

Zres %>%
  group_by(Publication, Size_Class) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResT, na.rm = TRUE),
    sd = sd(StNetResT, na.rm = TRUE))%>%
  ungroup()

#t tests

#overall

dfZR %>%  #ttest - calc vs true SRL
  do(tidy(t.test(.$StResS,
                 .$StResT,
                 mu = 0,
                 alt = "two.sided",
                 paired = TRUE,
                 conf.level = 0.99)))

dfZR %>%  #ttest - calc vs true SRL
  do(tidy(wilcox.test(.$TLCalc_SRL,
                      .$TLCalc_TRL,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      conf.level = 0.99)))

dfZR %>%  #ttest - calc vs true SRL
  do(tidy(wilcox.test(.$NetRes_S,
                 .$NetRes_T,
                 mu = 0,
                 alt = "two.sided",
                 exact = FALSE,
                 paired = TRUE,
                 conf.level = 0.99)))

#size/maturity

dfZR %>%  #ttest - calc vs true SRL
  group_by(Size_Class) %>%
  do(tidy(wilcox.test(.$NetRes_S,
                      .$NetRes_T,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      conf.level = 0.99)))%>%
ungroup()

dfZR %>%  #ttest - calc vs true SRL
  group_by(Size_Class) %>%
  do(tidy(t.test(.$StNetResS,
                 .$StNetResT,
                 mu = 0,
                 alt = "two.sided",
                 paired = TRUE,
                 conf.level = 0.99)))%>%
ungroup()

dfZR %>%  #ttest - calc vs true SRL
  group_by(Maturity) %>%
  do(tidy(wilcox.test(.$NetRes_S,
                      .$NetRes_T,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      conf.level = 0.99)))%>%
  ungroup()

dfZR %>%  #ttest - calc vs true SRL
  group_by(Maturity) %>%
  do(tidy(t.test(.$StNetResS,
                 .$StNetResT,
                 mu = 0,
                 alt = "two.sided",
                 paired = TRUE,
                 conf.level = 0.99)))%>%
  ungroup()

dfZR %>%  #ttest - calc vs true SRL
  group_by(Age_Class) %>%
  do(tidy(t.test(.$StNetResS,
                 .$StNetResT,
                 mu = 0,
                 alt = "two.sided",
                 paired = TRUE,
                 conf.level = 0.99)))%>%
  ungroup()

#publication

dfZR %>%  #ttest - calc vs true SRL
  group_by(Publication) %>%
  do(tidy(wilcox.test(.$NetRes_S,
                      .$NetRes_T,
                      mu = 0,
                      alt = "less",
                      paired = TRUE,
                      exact = FALSE,
                      conf.level = 0.99))) %>%
  ungroup()

dfZR %>%  #ttest - calc vs true SRL
  group_by(Publication) %>%
  do(tidy(t.test(.$StNetResS,
                 .$StNetResT,
                 mu = 0,
                 alt = "two.sided",
                 paired = TRUE,
                 exact = FALSE,
                 conf.level = 0.99))) %>%
  ungroup()

dfZR %>%  #ttest - calc vs true SRL
  group_by(Publication) %>%
  do(tidy(wilcox.test(.$TLCalc_SRL,
                      .$TLCalc_TRL,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      exact = FALSE,
                      conf.level = 0.99))) %>%
  ungroup()

#interaction

dfZR %>%  #ttest - calc vs true SRL
  group_by(Publication, Size_Class) %>%
  do(tidy(t.test(.$StNetResS,
                 .$StNetResT,
                 mu = 0,
                 alt = "greater",
                 paired = TRUE,
                 exact = FALSE,
                 conf.level = 0.99))) %>%
  ungroup()

dfZR %>%  #ttest - calc vs true SRL
  group_by(Publication, Maturity) %>%
  do(tidy(t.test(.$StNetResS,
                 .$StNetResT,
                 mu = 0,
                 alt = "less",
                 paired = TRUE,
                 exact = FALSE,
                 conf.level = 0.99))) %>%
  ungroup()

####Stacked Bar----

ZSNRP <- data_summary(Zres, varname = "StNetResS", 
                      groupnames=c("Publication", "Maturity", "ModelT"))

ZSNRP$Maturity=as.factor(ZSNRP$Maturity)
head(ZSNRP)

StBarZR <- ggplot(ZSNRP[order(ZSNRP$Maturity,decreasing=T),],
                  aes(x=reorder(Publication, +StNetResS),
                      y=StNetResS, 
                      fill=factor(Maturity, levels = c("Mature", "Immature")),
                      pattern = ModelT)) + 
  geom_bar(stat="identity", 
           position="stack") +
  geom_bar_pattern(stat="identity",
                   position = "stack",
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.09,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(values = c(Lin = "stripe", Pow = "none"),
                       labels = c('Linear', 'Power')) +
  geom_text(aes(label = paste(round(StNetResS,1),"±", round(sd,1))),
            position = position_stack(), hjust = 1.2, size = 3, fontface = 'bold', color = "white") +
  labs(title = "SRL Input", x = "Publication", y = "Standardised Net Residuals Produced", pattern = "Model Type") +
  theme_minimal() +
  scale_x_discrete(label = c("Faria et al. (2007) - Power", 
                             expression(paste(bold("Lear et al. (2023) - Female"))),
                             "Whitty et al. (2014) - Linear",
                             "Faria et al. (2007) - Linear",
                             expression(paste(bold("Lear et al. (2023) - Male"))),
                             "Peverell (2010)",
                             expression(paste(bold("*Whitty et al. (2014) - Power")))
  )) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))+
  theme(plot.title = element_text(face = "italic"), 
        legend.position = c(0.82, 0.25), 
        legend.background = element_rect(fill="white")) +
  coord_flip() +
  ylim(0, 47) +
  scale_fill_manual(name = "Maturity",
                    labels=c('Mature', 'Immature'), 
                    values = c("grey40", "grey70"))
StBarZR

ZSNRT <- data_summary(Zres, varname = "StNetResT", 
                      groupnames=c("Publication", "Maturity", "ModelT"))

ZSNRT$Maturity=as.factor(ZSNRT$Maturity)
head(ZSNRT)

ZSNRT$Publication <- factor(ZSNRT$Publication,                                    # Change ordering manually
                            levels = c('Faria Power',
                                       'Lear Female',
                                       'Whitty Linear',
                                       'Faria Linear',
                                       'Lear Male',
                                       'Peverell',
                                       'Whitty Power'))

StBarZRT <- ggplot(ZSNRT[order(ZSNRT$Maturity,decreasing=T),],
                   aes(x=Publication,
                       y=StNetResT, 
                       fill=factor(Maturity, levels = c("Mature", "Immature")),
                       pattern = ModelT)) + 
  geom_bar(stat="identity", 
           position="stack") +
  geom_bar_pattern(stat="identity",
                   position = "stack",
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.09,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(values = c(Lin = "stripe", Pow = "none"),
                       labels = c('Linear', 'Power')) +
  geom_text(aes(label = paste(round(StNetResT,1),"±", round(sd,1))),
            position = position_stack(), hjust = 1.2, size = 3, fontface = 'bold', color = "white") +
  labs(title = "TRL Input", x = "Publication", y = "Standardised Net Residuals Produced", pattern = "Model Type") +
  scale_x_discrete(label = c("Faria et al. (2007) - Power", 
                             expression(paste(bold("Lear et al. (2023) - Female"))),
                             "Whitty et al. (2014) - Linear",
                             "Faria et al. (2007) - Linear",
                             expression(paste(bold("Lear et al. (2023) - Male"))),
                             "Peverell (2010)",
                             expression(paste(bold("*Whitty et al. (2014) - Power")))
  )) +
  theme_minimal() +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))+
  theme(plot.title = element_text(face = "italic"), 
        legend.position = c(0.8, 0.75), 
        legend.background = element_rect(fill="white")) +
  coord_flip() +
  ylim(0, 47) +
  scale_fill_manual(name = "Maturity",
                    labels=c('Mature', 'Immature'), 
                    values = c("grey40", "grey70"))
StBarZRT

#TRL vs SRL - Pristis discussion
cowplot::plot_grid(StBarZR + theme(axis.text.x = element_blank(),
                                   axis.title.x = element_blank(),
                                   legend.position = "none"),
                   StBarZRT,
                   labels = "AUTO", ncol = 1, align ="v",
                   rel_heights = c(.85,1))

ggsave(
  "ZijTvS.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 10,
  height = 5,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

##Residuals by Age----
sumtable(Zres, group = "Age_Class", group.long=T) #as Table

####Means----
library("dplyr")
Zres %>%
  group_by(Age_Class) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(NetRes_T, na.rm = TRUE),
    sd = sd(NetRes_T, na.rm = TRUE)
  )%>%
  ungroup()

Zres %>%
  group_by(Age_Class) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(NetRes_S, na.rm = TRUE),
    sd = sd(NetRes_S, na.rm = TRUE)
  )%>%
  ungroup()

Zres %>%
  group_by(Age_Class) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(Residuals_SRL, na.rm = TRUE),
    sd = sd(Residuals_SRL, na.rm = TRUE)
  )%>%
  ungroup()

Zres %>%
  group_by(Age_Class) %>% #means Residuals by group TRL
  dplyr::summarise(
    count = n(),
    mean = mean(Residuals_TRL, na.rm = TRUE),
    sd = sd(Residuals_TRL, na.rm = TRUE)
  )%>%
  ungroup()

####t-test----

wtZSRLAge1 <- Zres %>% #wilcoxtest
  group_by(Age_Class) %>%
  do(tidy(wilcox.test(.$TLCalc_SRL,
                      .$TLTrue,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      conf.level = 0.99))) %>%
  ungroup()
wtZSRLAge1

wtZSRLAge2 <- Zres %>% #wilcoxtest
  group_by(Maturity) %>%
  do(tidy(wilcox.test(.$TLCalc_SRL,
                      .$TLTrue,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      conf.level = 0.99))) %>%
  ungroup()
wtZSRLAge2

wtZTRLAge1 <- Zres %>% #wilcoxtest
  group_by(Age_Class) %>%
  do(tidy(wilcox.test(.$TLCalc_TRL,
                      .$TLTrue,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      conf.level = 0.99))) %>%
  ungroup()
wtZTRLAge1

####ANOVA Age----
aovZAge <- aov(Residuals_SRL ~ Age_Class, Zres)
summary(aovPAge)

kruskal.test(Residuals_SRL ~ Age_Class, Zres)
kruskal.test(NetRes_S ~ Age_Class, Zres)
kruskal.test(SSq_SRL ~ Age_Class, Zres)

####Boxplot----
ggplot(Zres, aes(Age_Class, Residuals_SRL, color=Age_Class)) +
  geom_boxplot() +
  #coord_flip() +
  labs(title = "Projected Total Length of Zijsron SRL ", 
       x= bquote(NULL),
       y= bquote("Residuals"))

ggplot(Zres, aes(Age_Class, Residuals_TRL, color=Age_Class)) +
  geom_boxplot() +
  #coord_flip() +
  labs(title = "Projected Total Length of Zijsron TRL ", 
       x= bquote(NULL),
       y= bquote("Residuals"))

##Residuals by Pub----
sumtable(Zres, group = "Publication", group.long=T) #as Table

####Means by Pub----
Zres %>%
  group_by(Publication) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(Residuals_SRL, na.rm = TRUE),
    sd = sd(Residuals_SRL, na.rm = TRUE)
  )%>%
  ungroup()

Zres %>%
  group_by(Publication) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(NetRes_S, na.rm = TRUE),
    sd = sd(NetRes_S, na.rm = TRUE)
  )%>%
  ungroup()

 Zres %>%
  group_by(Publication) %>% #means Residuals by group TRL
   dplyr::summarise(
     count = n(),
    mean = mean(NetRes_T, na.rm = TRUE),
    sd = sd(NetRes_T, na.rm = TRUE)
  ) %>%
  ungroup()
 
 Zres %>%
   group_by(Publication) %>% #means Residuals by group SRL
   dplyr::summarise(
     count = n(),
     mean = mean(StNetResS, na.rm = TRUE),
     sd = sd(StNetResS, na.rm = TRUE)
   )%>%
   ungroup()

####t-tests----
 library(tidyverse)
 library(tidyr)
 library(dplyr)
 
pwZSRL <- Zres %>%  #ttest - calc vs true SRL
  group_by(Publication) %>%
  do(tidy(wilcox.test(.$TLCalc_SRL,
                 .$TLTrue,
                 mu = 0,
                 alt = "two.sided",
                 paired = TRUE,
                 conf.level = 0.99))) %>%
  ungroup()
pwZSRL

pwZSRL1 <- Zres %>%  #ttest - calc vs true SRL
  group_by(Publication, Age_Class) %>%
  do(tidy(wilcox.test(.$TLCalc_SRL,
                 .$TLTrue,
                 mu = 0,
                 alt = "two.sided",
                 paired = TRUE,
                 conf.level = 0.99))) %>%
  ungroup()
print(pwZSRL1, n = Inf)

pwZSRL3 <- Zres %>%  #ttest - calc vs true SRL
  group_by(Publication, Maturity) %>%
  do(tidy(wilcox.test(.$TLCalc_SRL,
                 .$TLTrue,
                 mu = 0,
                 alt = "two.sided",
                 paired = TRUE,
                 conf.level = 0.99))) %>%
  ungroup()
pwZSRL3

pwZTRL <- Zres %>%  #ttest - calc vs true TRL
  group_by(Publication) %>%
  do(tidy(t.test(.$TLCalc_TRL,
                 .$TLTrue,
                 mu = 0,
                 alt = "two.sided",
                 paired = TRUE,
                 conf.level = 0.99))) %>%
  ungroup()
pwZTRL

Ztest %>%  #ttest - calc vs true TRL
  group_by(Size_Class) %>%
  do(tidy(t.test(.$NetFL,
                 .$NetP,
                 mu = 0,
                 alt = "less",
                 paired = TRUE,
                 conf.level = 0.99))) %>%
  ungroup()

Ztest %>%  #ttest - calc vs true TRL
  group_by(Size_Class) %>%
  do(tidy(t.test(.$ResidKF,
                 .$ResidWL,
                 mu = 0,
                 alt = "two.sided",
                 paired = TRUE,
                 conf.level = 0.99))) %>%
  ungroup()
  
####ANOVA Pub----

aovZPUB <- aov(logNetRS ~ Publication + Size_Class + Publication:Size_Class, Zres)
summary(aovZPUB)
aovZ2PUB <- aov(StNetResS ~ Publication + Size_Class + Publication:Size_Class, Zres) #significant
summary(aovZ2PUB)
aovZ3PUB <- aov(StNetResS ~ Publication + Age_Class + Publication:Age_Class, Zres) #significant
summary(aovZ3PUB)
aovZ4PUB <- aov(logNetRS ~ Publication + Age_Class + Publication:Age_Class, Zres) #v significant!
summary(aovZ4PUB)

kruskal.test(NetRes_S ~ Publication, Zres)
kruskal.test(StNetResS ~ Publication, Zres)

####Zijsron Resid Corr----
sapply(
  split(data.frame(Zres$TLTrue, Zres$TLCalc_SRL), Zres$Publication), 
  function(x) cor(x[[1]],x[[2]])
)

##Association Age-Pub----

####Means----
ZAP1<- Zres %>%
  group_by(Age_Class, Publication) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(NetRes_S, na.rm = TRUE),
    sd = sd(NetRes_S, na.rm = TRUE)) %>%
  ungroup()
print(ZAP1, n = Inf)

ZAP2<- Zres %>%
  group_by(Age_Class, Publication) %>% #means Residuals by group Pub
  summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)) %>%
  ungroup()
print(ZAP2, n = Inf)

Zres %>%
  group_by(Maturity, Publication) %>% #means Residuals by group Pub
  summarise(
    count = n(),
    mean = mean(NetRes_S, na.rm = TRUE),
    sd = sd(NetRes_S, na.rm = TRUE)) %>%
  ungroup()

Zres %>%
  group_by(Maturity, Publication) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)) %>%
  ungroup()

Zres %>%
  group_by(Size_Class, Publication) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(NetRes_S, na.rm = TRUE),
    sd = sd(NetRes_S, na.rm = TRUE)) %>%
  ungroup()

Zres %>%
  group_by(Size_Class, Publication) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)) %>%
  ungroup()

####t-Test split----

t.test(formula = LogP ~ Maturity, data = dfZT) #significant
t.test(formula = LogFL ~ Maturity, data = dfZT) #significant
t.test(formula = LogFP ~ Maturity, data = dfZT) #significant
t.test(formula = LogKF ~ Maturity, data = dfZT) #significant
t.test(formula = LogKM ~ Maturity, data = dfZT) #significant
t.test(formula = LogWC ~ Maturity, data = dfZT) #significant
t.test(formula = LogWL ~ Maturity, data = dfZT) #significant

t.test(formula = SNStP ~ Maturity, data = dfZT) #notsignificant
t.test(formula = SNStFL ~ Maturity, data = dfZT) #notsignificant
t.test(formula = SNStFP ~ Maturity, data = dfZT) #notsignificant
t.test(formula = SNStKF ~ Maturity, data = dfZT) ##notsignificant
t.test(formula = SNStKM ~ Maturity, data = dfZT) #notsignificant
t.test(formula = SNStWL ~ Maturity, data = dfZT) #notsignificant
t.test(formula = SNStWC ~ Maturity, data = dfZT) #significant

####ANOVA Split by Pubs----

summary(aov(SNStP ~ Age_Class, data = Ztest)) #1Way
summary(aov(SNStFL ~ Age_Class, data = Ztest)) #1Way
summary(aov(SNStFP ~ Age_Class, data = Ztest)) #1Way
summary(aov(SNStKF ~ Age_Class, data = Ztest)) #1Way
summary(aov(SNStKM ~ Age_Class, data = Ztest)) #1Way
summary(aov(SNStWL ~ Age_Class, data = Ztest)) #1Way
summary(aov(SNStWC ~ Age_Class, data = Ztest)) #1Way

####Intxn Plot----

interaction.plot(x.factor     = factor(Zres$Age_Class, levels = c("YOY", "juvenile", "sub-adult", "adult")),
                 trace.factor = Zres$Publication,
                 response     = Zres$StNetResS,
                 fun = mean,
                 type="b",
                 col=c("black",
                       "green",
                       "red",
                       "purple", 
                       "blue",
                       "yellow", 
                       "orange"),  ### Colors for levels of trace var.
                 pch=c(19, 15, 15, 15, 19, 19, 15),  ### Symbols for levels of trace var.
                 fixed=TRUE,                         ### Order by factor order in data
                 leg.bty = "o")

modelZ2 = lm(StNetResS ~ Publication + Age_Class + Publication:Age_Class, data = Zres)
Anova(modelZ2, type = "II")

modelZ3 = lm(StNetResS ~ Publication + Maturity + Publication:Maturity, data = Zres)
Anova(modelZ3, type = "II")

####State----
modelZ4 = lm(SRL_TL ~ State + Maturity + State:Maturity, data = PZ)
Anova(modelZ4, type = "II")

modelZ5 = lm(SRL_TL ~ State + Age_Class + State:Age_Class, data = PZ)
Anova(modelZ5, type = "II")

####Boxplot----
ggplot(Zres, aes(Age_Class, Residuals_SRL, color=Publication)) +
  geom_boxplot() +
  #coord_flip() +
  labs(title = "Projected Total Length of Zijsron ", 
       x= bquote(NULL),
       y= bquote("Residuals"))

ggplot(Zres, aes(Maturity, Residuals_SRL, color=Publication)) +
  geom_boxplot() +
  #coord_flip() +
  labs(title = "Projected Total Length of Zijsron ", 
       x= bquote(NULL),
       y= bquote("Residuals"))

##Gradient----

modAP <- lm(SStP ~ TLCorr, data = Atest)
summary(modAP)

modZP <- lm(SStP ~ TLCorr, data = Ztest)
modZFL <- lm(SStFL ~ TLCorr, data = Ztest)
modZFP <- lm(SStFP~ TLCorr, data = Ztest)
modZLF <- lm(SStKF~ TLCorr, data = Ztest)
modZLM <- lm(SStKM~ TLCorr, data = Ztest)
modZWL <- lm(SStWL~ TLCorr, data = Ztest)
modZWP<- lm(SStWC~ TLCorr, data = Ztest)

modelsZPub <- list(modZP, modZFL, modZFP, modZLF, modZLM, modZWL, modZWP)
modZnam <- c('P', 'FL', 'FP', 'LF', 'LM', 'WL', 'WP')

summary(modZP)
summary(modZFL)
summary(modZFP)
summary(modZLF)
summary(modZLM)
summary(modZWL)
summary(modZWP)


##ToPrint----
####AgeBarPlot----
ZRP <- data_summary(Zres, varname = "Residuals_SRL", 
                    groupnames=c("Publication", "Age_Class"))

ZRP$Age_Class=factor(ZRP$Age_Class, levels = c("adult", "sub-adult", "juvenile", "YOY"))
head(ZRP)

PZwrap <- c("Faria (2007) Linear",
            "Faria (2007) Power",
            "Peverell (2010)",
            "Whitty et al. (2014) Ratio",
            "Lear et al. (2023) - Female",
            "Lear et al. (2023) - Male",
            "Whitty et al. (2014) Power")

wrap_labelsZ <- function(x) {
  str_wrap(PZwrap, width = 15)
}


BarZR <- ggplot(ZRP, aes(x=factor(Publication, level = c("Faria Linear",
                                                         "Faria Power",
                                                         "Peverell",
                                                         "Whitty Linear",
                                                         "Lear Female",
                                                         "Lear Male",
                                                         "Whitty Power")), 
                        y=Residuals_SRL, 
                        fill=factor(Age_Class, level = c('YOY',
                                                         'juvenile',
                                                         'sub-adult',
                                                         'adult')))) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Residuals_SRL-sd, ymax=Residuals_SRL+sd), width=.2,
                position=position_dodge(.9)) +
  labs(title = "P. zijsron", x = "Model", y = "Residuals (mm)") +
  theme(plot.title = element_text(face = "italic"),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  scale_fill_manual(name = "Size Class",
                    labels = c('YOY', 'Juvenile', 'Sub-adult', 'Adult'), 
                    values = c("#5ab4ac", "#f5f5f5", "56B4E9", "#d8b365")) +
  scale_x_discrete(labels = c("FL", "FP", "P", "WR", "LF", "LM", "W")) 
print(BarZR)

# Finished bar plot

####DotPlot----
PZDP <- ggplot(Zres, mapping=aes(x=TLTrue, 
                                 y=StResS, 
                                 color=factor(Publication, 
                                              levels = c("Whitty Linear",
                                                         "Faria Power",
                                                         "Lear Female",
                                                         "Lear Male",
                                                         "Faria Linear",
                                                         "Peverell",
                                                         "Whitty Power")))) +
  coord_cartesian(xlim =c(800, 5400)) +
  geom_point(size = 0.75) +
  geom_hline(yintercept = c(-5, 0, 5),
             linetype = c('dotted', "longdash", 'dotted'),
             color = c('grey40', 'black', 'grey40')) +
  annotate("rect", xmin = -Inf, xmax = Inf, 
           ymin = -5.2, ymax= 5.2, alpha = 0.1, fill = 'grey40') +
  scale_color_manual(name = "Model",
                     labels=c('Whitty et al. (2014) Ratio',
                              'Faria (2007) Power', 
                              "Lear et al. (2023) - Female",
                              "Lear et al. (2023) - Male",
                              'Faria (2007) Linear', 
                              'Peverell (2010)',
                              'Whitty et al. (2014) Power'),
                     values = c('darkblue',
                                'yellow',
                                'darkgreen', 
                                'lightgreen',  
                                'orange',
                                rgb(0.2,0.4,0.1,0.7),
                                'lightblue')) +
  stat_smooth(aes(x=TLTrue, y=StResS, color=Publication), 
              method = lm, se = FALSE) +
  geom_vline(xintercept = c(760, 1300, 3000, 3800, 5400),
             linetype = c('solid', 'solid', 'dotdash', 'solid', 'solid')) +
  # annotate(geom = "text",
  #          label = c('Birth Size', 'One Year', ' ', 'Maturity', 'Max Size'),
  #          x = c(760, 1300, 3000, 3800, 5400),
  #          y = c(27, 27, 27, 27, 27),
  #          size = c(4,4,4,4,4),
  #          angle = 90,
  #          vjust = -1) +
  geom_segment(x = 760, y = 34, xend = 1300, yend = 34,
               colour = "#5ab4ac",
               linewidth = 3) +
  geom_segment(x = 1300, y = 34, xend = 3000, yend = 34,
               colour = "#f5f5f5",
               linewidth = 3) +
  geom_segment(x = 3000, y = 34, xend = 3800, yend = 34,
               colour = "lightgrey",
               linewidth = 3) +
  geom_segment(x = 3800, y = 34, xend = 5400, yend = 34,
               colour = "#d8b365",
               linewidth = 3) +
  # annotate(geom = "label",
  #          label = c('YOY', 'Juvenile', 'Sub-Adult', 'Adult'),
  #          x = c((760+1300)/2, (1300+3000)/2, (3000+3800)/2, (3800+5400)/2),
  #          y = c(47, 47, 47, 47),
  #          size = c(4,4,4,4),
  #          fill = c("#5ab4ac", "#f5f5f5", "lightgrey", "#d8b365"),
  #          colour = c("white", "black", "black", "white"),
  #          angle = 0,
  #          vjust = 1) +
  theme(plot.title = element_text(face = "italic"),
        legend.key = element_rect(fill = "white"),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = c(0.325, 0.675)) +
  labs(title = "Pristis zijsron",
       x = bquote("Total Length (mm)"),
       y = bquote("Percent Error (%)"),
       color = "Model")
PZDP

ggplot(Zres, mapping=aes(x=TLTrue, y=StResS, color=Publication)) +
  geom_point() +
  geom_hline(yintercept=0,linetype="dashed") +
  stat_ellipse(aes(x=TLTrue, y=StResS, color=Publication),type = "norm") +
  scale_fill_manual(values = c("blue4","gold2", "red3", "yellow", "darkgreen", "grey", "light grey")) +
  labs(title = "Zijsron Standardised",
       x = bquote("Total Length"),
       y = bquote("Residuals"),
       fill = "Publication")

####St Res Bar----
ZSNRSC <- data_summary(Zres, varname = "StNetResS", 
                       groupnames=c("Publication", "Maturity", "ModelT"))

ZSNRSC$Size_Class=as.factor(ZSNRSC$Maturity)
head(ZSNRSC)

#Lets look at size splits (even data) instead of maturity

StBarZSize <- ggplot(ZSNRSC[order(ZSNRSC$Maturity,decreasing=T),],
                     aes(x=reorder(Publication, +StNetResS),
                         y=StNetResS, 
                         fill=factor(Size_Class, levels = c("Mature", "Imature")),
                         pattern = ModelT)) + 
  geom_bar(stat="identity", 
           position="stack") +
  geom_bar_pattern(stat="identity",
                   position = "stack",
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.09,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(values = c(Lin = "stripe", Pow = "none"),
                       labels = c('Linear', 'Power')) +
  geom_text(aes(label = paste(round(StNetResS,1),"±", round(sd,1))),
            position = position_stack(), hjust = 1.2, size = 3, color = "white") +
  labs(title = "SRL Input", x = "Publication", y = "Standardised Net Residuals Produced", pattern = "Model Type") +
  theme_minimal() +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))+
  theme(plot.title = element_text(face = "italic"), 
        legend.position = c(0.82, 0.4), 
        legend.background = element_rect(fill="white")) +
  coord_flip() +
  ylim(0, 25) +
  scale_x_discrete(labels  = c("Faria (2007) Power",
                               "Lear et al. (2023) Female",
                               "Whitty et al. (2014) Linear",
                               "Faria (2007) Linear",
                               "Lear et al. (2007) Male",
                               "Peverell (2010)",
                               "Whitty et al. (2014) Power"
  )) +
  scale_fill_manual(name = "Maturity",
                    labels=c('Mature', 'Immature'), 
                    values = c("grey30", "grey80"))
StBarZSize

####Pub Section Pic----
#Dot + Box

plots <- align_plots(BarZR + theme(plot.title = element_blank(),
                                   legend.position = "none"), 
                     PZDP + theme(plot.title = element_blank()), 
                     align = 'v', axis = 'l')
topZ <- plot_grid(plots[[1]], StBarZSize + theme(plot.title = element_blank()),
                  labels = c('A', 'B'))

cowplot::plot_grid(topZ, plots[[2]],
                   labels = c('','C'), nrow = 2, rel_heights = c(1.25,2))

cowplot::plot_grid(BarZR + theme(plot.title = element_blank(),
                                 legend.position = "none"),
                   PZDP + theme(plot.title = element_blank()),
                   labels = "auto", ncol = 1, rel_heights = c(1,2), align ="v", axis = 'l')

ggsave(
  "ZijjyData.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 12,
  height = 9,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

##Residual Plot----
####Big Boi----
library(cowplot)
Za <- ggplot(dfZT, aes(x = SRL, y = TLCorr)) + #Pev
  geom_smooth(aes(x = SRL, y = TL_Pev_SRL), method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = SRL, yend = TL_Pev_SRL), alpha = .2) +
  ylim(0, 6200) +
  geom_point(aes(color = ResidP)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  geom_point(aes(y = TLCorr), shape = 1) +
  labs(x= "SRL", y = "Total Length", 
       title= "Peverell, 2010", color ="Residuals") +
  theme_bw()
Zb <- ggplot(dfZT, aes(x = SRL, y = TLCorr)) + #FariaL
  geom_smooth(aes(x = SRL, y = TL_FarL_SRL), method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = SRL, yend = TL_FarL_SRL), alpha = .2) +
  ylim(0, 6200) +
  geom_point(aes(color = ResidFL)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  geom_point(aes(y = TLCorr), shape = 1) +
  labs(x= "SRL", y = "Total Length", 
       title= "Faria, 2007 - Linear", color ="Residuals") +
  theme_bw()
Zc <- ggplot(dfZT, aes(x = SRL, y = TLCorr)) + #FariaE
  stat_smooth(aes(x = SRL, y = TL_FarE_SRL), method = "loess", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = SRL, yend = TL_FarE_SRL), alpha = .2) +
  ylim(0, 6200) +
  geom_point(aes(color = ResidFE)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  geom_point(aes(y = TLCorr), shape = 1) +
  labs(x= "SRL", y = "Total Length", 
       title= "Faria, 2007 - Power", color ="Residuals") +
  theme_bw()
Zd <- ggplot(dfZT, aes(x = SRL, y = TLCorr)) + #WhittyP
  stat_smooth(aes(x = SRL, y = TL_WP_SRL), method = "loess", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = SRL, yend = TL_WP_SRL), alpha = .2) +
  ylim(0, 6200) +
  geom_point(aes(color = ResidWP)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  geom_point(aes(y = TLCorr), shape = 1) +
  labs(x= "SRL", y = "Total Length", 
       title= "Whitty et al. 2014 - Power", color ="Residuals") +
  theme_bw()
Ze <- ggplot(dfZT, aes(x = SRL, y = TLCorr)) + #WhittyL
  geom_smooth(aes(x = SRL, y = TL_WL_SRL), method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = SRL, yend = TL_WL_SRL), alpha = .2) +
  ylim(0, 6200) +
  geom_point(aes(color = ResidWL)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  geom_point(aes(y = TLCorr), shape = 1) +
  labs(x= "SRL", y = "Total Length", 
       title= "Whitty et al. 2014 - Ratio", color ="Residuals") +
  theme_bw()
Zf <- ggplot(dfZT, aes(x = SRL, y = TLCorr)) + 
  geom_smooth(aes(x = SRL, y = KLF), method = "loess", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = SRL, yend = KLF), alpha = .2) +
  ylim(0, 6200) +
  geom_point(aes(color = ResidKF)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  geom_point(aes(y = TLCorr), shape = 1) +
  labs(x= "SRL", y = "Total Length", 
       title= "Lear et al. 2023 - Female", color ="Residuals") +
  theme_bw()
Zg <- ggplot(dfZT, aes(x = SRL, y = TLCorr)) + 
  geom_smooth(aes(x = SRL, y = KLM), method = "loess", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = SRL, yend = KLM), alpha = .2) +
  ylim(0, 6200) +
  geom_point(aes(color = ResidKM)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  geom_point(aes(y = TLCorr), shape = 1) +
  labs(x= "SRL", y = "Total Length", 
       title= "Lear et al. 2023 - Male", color ="Residuals") +
  theme_bw()

ZResGALL <- cowplot::plot_grid(Zb, 
                               Ze + theme(axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank(),
                                          axis.title.y = element_blank() ),
                               Za + theme(axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank(),
                                          axis.title.y = element_blank() ),
                               Zc,
                               Zd+ theme(axis.text.y = element_blank(),
                                         axis.ticks.y = element_blank(),
                                         axis.title.y = element_blank() ),
                               NULL,
                               Zf,
                               Zg + theme(axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank(),
                                          axis.title.y = element_blank() ),
                               labels = c('A','','','B','','','C',''), ncol = 3, align ="v")
ZResGALL

ggsave(
  "ZijsronRes.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 12,
  height = 9,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

###AMSAPoster----

PZDPoster <- ggplot(Zres, mapping=aes(x=TLTrue, 
                                      y=StResS, 
                                      color=factor(Publication, 
                                                   levels = c("Faria Power",
                                                              "Lear Female",
                                                              "Whitty Linear",
                                                              "Faria Linear",
                                                              "Peverell",
                                                              "Lear Male",
                                                              "Whitty Power")))) +
  coord_cartesian(xlim =c(600, 5500)) +
  geom_point() +
  geom_hline(yintercept = c(-5, 0, 5),
             linetype = c('dotted', "longdash", 'dotted'),
             color = c('grey40', 'black', 'grey40')) +
  annotate("rect", xmin = -Inf, xmax = Inf, 
           ymin = -5.2, ymax= 5.2, alpha = 0.1, fill = 'grey40') +
  scale_color_manual(name = "Model",
                     labels=c('Faria (2007) Power', 
                              "Lear et al. (2023) - Female",
                              'Whitty et al. (2014) Ratio',
                              'Faria (2007) Linear', 
                              'Peverell (2010)',
                              "Lear et al. (2023) - Male",
                              'Whitty et al. (2014) Power'),
                     values = c('yellow',
                                'darkgreen', 
                                'darkblue',
                                'orange',
                                rgb(0.2,0.4,0.1,0.7), 
                                'lightgreen', 
                                'lightblue')) +
  stat_smooth(aes(x=TLTrue, y=StResS, color=Publication), 
              method = lm, se = FALSE) +
  geom_vline(xintercept = c(760, 1300, 3000, 3800, 5400),
             linetype = c('solid', 'solid', 'dotdash', 'solid', 'solid')) +
  annotate(geom = "text",
           label = c('Birth Size', 'One Year', ' ', 'Maturity', 'Max Size'),
           x = c(760, 1300, 3000, 3800, 5400),
           y = c(30, 30, 40, 30, 30),
           angle = 90,
           vjust = -1) +
  geom_segment(x = 760, y = 40, xend = 1300, yend = 40,
               colour = "#5ab4ac",
               linewidth = 2) +
  geom_segment(x = 1300, y = 40, xend = 3000, yend = 40,
               colour = "#f5f5f5",
               linewidth = 2) +
  geom_segment(x = 3000, y = 40, xend = 3800, yend = 40,
               colour = "lightgrey",
               linewidth = 2) +
  geom_segment(x = 3800, y = 40, xend = 5400, yend = 40,
               colour = "#d8b365",
               linewidth = 2) +
  annotate(geom = "label",
           label = c('YOY', 'Juvenile', 'Sub-Adult', 'Adult'),
           x = c((760+1300)/2, (1300+3000)/2, (3000+3800)/2, (3800+5400)/2),
           y = c(42, 42, 42, 42),
           fill = c("#5ab4ac", "#f5f5f5", "lightgrey", "#d8b365"),
           colour = c("white", "black", "black", "white"),
           angle = 0,
           vjust = 1) +
  theme(plot.title = element_text(face = "italic"),
        legend.key = element_rect(fill = "white"),
        legend.position = 'bottom') +
  labs(title = "Pristis zijsron",
       x = bquote("Total Length (mm)"),
       y = bquote("Percent Error"),
       color = "Publication")
PZDPoster

ggsave(
  "ZijAMSA.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8,
  height = 5,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

###ModelType----

interaction.plot(x.factor     = Zres$ModelT,
                 trace.factor = Zres$Maturity,
                 response     = Zres$StNetResS,
                 fun = mean,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")

modelZT = lm(StNetResS ~ ModelT + Maturity + ModelT:Maturity, data = Zres)
Anova(modelZT, type = "II")

#ALLTest----

##ResBar----

#AC

BarAR <- ggplot(ARP, aes(x=factor(Publication, level = c("Faria Linear",
                                                         "Morgan",
                                                         "Peverell",
                                                         "Whitty Linear",
                                                         "Faria Power",
                                                         "Lear",
                                                         "Thorburn",
                                                         "Whitty Quad")),
                         y=Residuals_SRL, 
                         fill=factor(Age_Class, level = c('YOY',
                                                          'juvenile',
                                                          'subadult',
                                                          'adult')))) + 
  geom_bar(stat="identity", 
           color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Residuals_SRL-sd, 
                    ymax=Residuals_SRL+sd), 
                width=.2,
                position=position_dodge(.9)) +
  labs(title = "A. cuspidata", x = "Publication", y = "Residuals") +
  theme(plot.title = element_text(face = "italic")) +
  scale_x_discrete(drop = FALSE,
                   labels = c("Faria et al. 2007",
                              '',
                              "Peverell, 2010",
                              "Whitty et al. 2014",
                              "Faria et al. 2007",
                              '',
                              '',
                              "Whitty et al. 2014")) +
  scale_fill_manual(drop = FALSE,
                    name = "Size Class",
                    labels=c('YOY', 'Juvenile', 'Sub-Adult' ,'Adult'), 
                    values = c("#5ab4ac", "#f5f5f5", 'grey', "#d8b365")) 

print(BarAR)

#PC

BarCR <- ggplot(CRP, aes(x=factor(Publication, level = c("Faria Linear",
                                                         "Morgan",
                                                         "Peverell",
                                                         "Whitty Linear",
                                                         "Faria Power",
                                                         "Lear",
                                                         "Thorburn",
                                                         "Whitty Power")),
                         y=Residuals_SRL,
                         fill=factor(Age_Class, level = c('YOY',
                                                          'juvenile',
                                                          'sub-adult',
                                                          'adult')))) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Residuals_SRL-sd, ymax=Residuals_SRL+sd), width=.2,
                position=position_dodge(.9)) +
  labs(title = "P. clavata", x = "Publication", y = "Residuals") +
  theme(plot.title = element_text(face = "italic")) +
  scale_fill_manual(name = "Size Class",
                    labels = c('YOY', 'Juvenile', 'Sub-adult', 'Adult'), 
                    values = c("#5ab4ac", "#f5f5f5", "56B4E9", "#d8b365")) +
  scale_x_discrete(drop = FALSE,
                   labels = c("Faria et al. 2007",
                              '',
                              "Peverell, 2010",
                              "Whitty et al. 2014",
                              "Faria et al. 2007",
                              '',
                              'Thorburn et al. 2007',
                              "Whitty et al. 2014")) 
print(BarCR)

#PP

BarPR <- ggplot(PRP, aes(x=factor(Publication, level = c("Faria Linear",
                                                         "Morgan",
                                                         "Peverell",
                                                         "Whitty Linear",
                                                         "Faria Power",
                                                         "Thorburn F",
                                                         "Thorburn M",
                                                         "Whitty Power")),
                         y=Residuals_SRL, 
                         fill=factor(Age_Class, level = c('YOY',
                                                          'juvenile',
                                                          'sub-adult',
                                                          'adult')))) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Residuals_SRL-sd, ymax=Residuals_SRL+sd), width=.2,
                position=position_dodge(.9)) +
  labs(title = "P. pristis", x = "Publication", y = "Residuals") +
  theme(plot.title = element_text(face = "italic")) +
  scale_x_discrete(drop = FALSE,
                   labels = c("Faria et al. 2007",
                              'Morgan et al. 2011',
                              "Peverell, 2010",
                              "Whitty et al. 2014",
                              "Faria et al. 2007",
                              'Thorburn et al. 2007 - Female',
                              'Thorburn et al. 2007 - Male',
                              "Whitty et al. 2014"))  +
  scale_fill_manual(name = "Size Class",
                    labels = c('YOY', 'Juvenile', 'Sub-adult', 'Adult'), 
                    values = c("#5ab4ac", "#f5f5f5", "56B4E9", "#d8b365")) 
print(BarPR)

#PZ

BarZR <- ggplot(ZRP, aes(x=factor(Publication, level = c("Faria Linear",
                                                         "Morgan",
                                                         "Peverell",
                                                         "Whitty Linear",
                                                         "Faria Power",
                                                         "Lear Female",
                                                         "Lear Male",
                                                         "Whitty Power")), 
                         y=Residuals_SRL, 
                         fill=factor(Age_Class, level = c('YOY',
                                                          'juvenile',
                                                          'sub-adult',
                                                          'adult')))) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Residuals_SRL-sd, ymax=Residuals_SRL+sd), width=.2,
                position=position_dodge(.9)) +
  labs(title = "P. zijsron", x = "Publication", y = "Residuals") +
  theme(plot.title = element_text(face = "italic")) +
  scale_fill_manual(name = "Size Class",
                    labels = c('YOY', 'Juvenile', 'Sub-adult', 'Adult'), 
                    values = c("#5ab4ac", "#f5f5f5", "56B4E9", "#d8b365")) +
  scale_x_discrete(drop = FALSE,
                   labels = c("Faria et al. 2007",
                              '',
                              "Peverell, 2010",
                              "Whitty et al. 2014",
                              "Faria et al. 2007",
                              'Lear et al. 2023 - Female',
                              'Lear et al. 2023 - Male',
                              "Whitty et al. 2014")) 
print(BarZR)

#Print

ResBar <- cowplot::plot_grid(BarAR + theme(axis.title.y = element_blank(),
                                           axis.title.x = element_blank(),
                                           legend.position = "none"), 
                             BarCR + theme(axis.title.y = element_blank(),
                                           axis.title.x = element_blank(),
                                           legend.position = "none"), 
                             BarPR + theme(axis.title.y = element_blank(),
                                           axis.title.x = element_blank(),
                                           legend.position = "none"), 
                             BarZR + theme(axis.title.y = element_blank(),
                                           axis.title.x = element_blank(),
                                           legend.position = "none"), 
                             labels = "AUTO", 
                             ncol = 1, align ="v")
ResBar

ggsave(
  "ResBer.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 15,
  height = 9,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

##PooledDot----
BigDots <- cowplot::plot_grid(ACDP, 
                              PCDP,
                              PPDP,
                              PZDP,
                              labels = "AUTO", ncol = 2, align ="v")
BigDots

ggsave(
  "BigDots.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 18,
  height = 8,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

##Fig4----
library(ggplot2)
library(cowplot)
library(rlang)

PLeg <- ggplot(Pres, mapping=aes(x=TLTrue, 
                                 y=StResS, 
                                 color=factor(Publication, level = c("Whitty Linear",
                                                                     "Thorburn F",
                                                                     "Peverell",
                                                                     "Morgan",
                                                                     "Faria Linear",
                                                                     "Whitty Power",
                                                                     "Thorburn M",
                                                                     "Faria Power")))) +
  coord_cartesian(xlim =c(400, 6350)) +
  geom_point() +
  scale_color_manual(labels=c('    ','    ','    ','    ',
                              '    ','    ','    ','    '),
                     values = c('darkblue',
                                'magenta',
                                rgb(0.2,0.4,0.1,0.7),
                                'red',
                                'orange',
                                'lightblue',
                                'purple',
                                'yellow')) +
  stat_smooth(aes(x=TLTrue, y=StResS, color=Publication), 
              method = lm, se = FALSE) +
  annotate(geom = "label",
           label = c('YOY', 'Juvenile', 'Sub-Adult', 'Adult'),
           x = c((760+1200)/2, (1200+2700)/2, (2700+3000)/2, (3000+6400)/2),
           y = c(52, 52, 52, 52),
           size = c(4,4,4,4),
           fill = c("#5ab4ac", "#f5f5f5", "lightgrey", "#d8b365"),
           colour = c("white", "black", "black", "white"),
           angle = 0,
           vjust = 1) +
  theme(plot.title = element_text(face = "italic"),
        legend.key = element_rect(fill = "white"),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.title = element_blank(),
        axis.title = element_text(size = 12),
        legend.position = "bottom",
        legend.spacing.x = unit(0.85, 'cm')) +
  guides(colour = guide_legend(nrow = 1))
PLeg

CLeg <- ggplot(Cres, mapping=aes(x=TLTrue, 
                                 y=StResS, 
                                 color=factor(Publication, level = c("Whitty Power",
                                                                     "Faria Power",
                                                                     "Faria Linear",
                                                                     "Thorburn",
                                                                     "Whitty Linear",
                                                                     "Peverell")))) +
  coord_cartesian(xlim =c(400, 6350)) +
  geom_point() +
  scale_color_manual(labels=c('   ','   ','   ','   ','   ', '   '),
                     values = c('lightblue',
                                'yellow',
                                'orange',
                                'plum3', 
                                'darkblue',
                                rgb(0.2,0.4,0.1,0.7))) +
  stat_smooth(aes(x=TLTrue, y=StResS, color=Publication), 
              method = lm, se = FALSE) +
  annotate(geom = "label",
           label = c('YOY', 'Juvenile', 'Sub-Adult', 'Adult'),
           x = c((760+1200)/2, (1200+2700)/2, (2700+3000)/2, (3000+6400)/2),
           y = c(52, 52, 52, 52),
           size = c(4,4,4,4),
           fill = c("#5ab4ac", "#f5f5f5", "lightgrey", "#d8b365"),
           colour = c("white", "black", "black", "white"),
           angle = 0,
           vjust = 1) +
  theme(plot.title = element_text(face = "italic"),
        legend.key = element_rect(fill = "white"),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "bottom",
        legend.spacing.x = unit(0.9, "cm")) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Pristis clavata",
       x = bquote("Total Length (mm)"),
       y = bquote("Percent Error (%)"),
       color = "Model")
CLeg

#need legend in between!

#align em
plotsL <- align_plots(BarCR1 + theme(axis.title.x = element_blank(),
                                    legend.position = "none"),
                      PCDP + theme(plot.title = element_blank(),
                                   legend.position = "none"),
                      align = 'v', axis = 'l')

plotsR <- align_plots(BarPR1 + theme(axis.title.x = element_blank(),
                                    axis.title.y = element_blank(),
                                    legend.position = "none"),
                      PPDP + theme(plot.title = element_blank(),
                                   axis.title.y = element_blank(),
                                   legend.position = "none"),
                      align = 'v', axis = 'l')

leg1 <- get_legend(CLeg)
leg2 <- get_legend(PLeg)

#leg in middle
legR <- plot_grid(leg1,
                  NULL,
                  leg2,
                  rel_widths = c(1.1, 0.05, 1.4),
                  align = "h",
                  nrow = 1)

#top row bars
topPCP <- plot_grid(plotsL[[1]], 
                    plotsR[[1]], 
                    labels = c('a)', 'c)'),
                    label_size = 13,
                    label_fontface = "plain",
                    rel_widths = c(1, 1.3),
                    align = 'h')

#bottom row dots
botPCP <- plot_grid(plotsL[[2]],
                    plotsR[[2]], 
                    labels = c('b)', 'd)'),
                    label_size = 13,
                    label_fontface = "plain",
                    rel_widths = c(1, 1.3))

cowplot::plot_grid(topPCP,
                   legR,
                   botPCP,
                   rel_heights = c(1,0.1,2),
                   align = 'h',
                   ncol = 1)

ggsave(
  "EZRes.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8,
  height = 5,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

#too big

#print legends sep and paste in

FourLeg <- ggplot(Pres, mapping=aes(x=TLTrue, 
                                 y=StResS, 
                                 color=factor(Publication, level = c("Whitty Linear",
                                                                     "Thorburn F",
                                                                     "Peverell",
                                                                     "Morgan",
                                                                     "Faria Linear",
                                                                     "Whitty Power",
                                                                     "Thorburn M",
                                                                     "Faria Power")))) +
  coord_cartesian(xlim =c(400, 6350)) +
  geom_point() +
  scale_color_manual(labels=c('FL = Faria (2007) Linear', 
                              'FP = Faria (2007) Power',
                              'M = Morgan et al. (2011)', 
                              'P = Peverell (2010)',
                              'TF = Thorburn et al. (2008) - Female',
                              'TM = Thorburn et al. (2008) - Male',
                              'WR = Whitty et al. (2014) - Ratio',
                              'W = Whitty et al. (2014)'),
                     values = c('orange',
                                'yellow',
                                'red',
                                rgb(0.2,0.4,0.1,0.7),
                                'magenta',
                                'purple',
                                'darkblue',
                                'lightblue')) +
  stat_smooth(aes(x=TLTrue, y=StResS, color=Publication), 
              method = lm, se = FALSE) +
  theme(plot.title = element_text(face = "italic"),
        legend.key = element_rect(fill = "white"),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.title = element_blank(),
        axis.title = element_text(size = 12),
        legend.position = "right",
        legend.spacing.x = unit(0.85, 'cm'))


print(FourLeg)

ggsave(
  "Fig5Leg.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8,
  height = 5,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

cowplot::plot_grid(topPCP,
                   botPCP,
                   rel_heights = c(1,1.75),
                   align = 'h',
                   ncol = 1)

##Fig5----

ZLeg <- ggplot(Zres, mapping=aes(x=TLTrue, 
                                 y=StResS, 
                                 color=factor(Publication, level = c("Faria Linear",
                                                                     "Faria Power",
                                                                     "Peverell",
                                                                     "Whitty Linear",
                                                                     "Lear Female",
                                                                     "Lear Male",
                                                                     "Whitty Power")))) +
  coord_cartesian(xlim =c(400, 6350)) +
  geom_point() +
  scale_color_manual(labels=c('FL = Faria (2007) Linear', 
                              'FP = Faria (2007) Power',
                              'P = Peverell (2010)',
                              'LF = Lear et al. (2023) - Female',
                              'LM = Lear et al. (2023) - Male',
                              'WR = Whitty et al. (2014) - Ratio',
                              'W = Whitty et al. (2014)'),
                     values = c('orange',
                                'yellow',
                                rgb(0.2,0.4,0.1,0.7),
                                'darkgreen',
                                'lightgreen',
                                'darkblue',
                                'lightblue')) +
  stat_smooth(aes(x=TLTrue, y=StResS, color=Publication), 
              method = lm, se = FALSE) +
  theme(plot.title = element_text(face = "italic"),
        legend.key = element_rect(fill = "white"),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.title = element_blank(),
        axis.title = element_text(size = 12),
        legend.position = "right",
        legend.spacing.x = unit(1, 'cm')) +
  labs(title = "P. zijsron",
       x = bquote("Total Length (mm)"),
       y = bquote("Percent Error (%)"),
       color = "Model")
ZLeg

ALeg <- ggplot(Ares, mapping=aes(x=TLTrue, 
                                 y=StResS, 
                                 color=factor(Publication, level = c("Whitty Linear",
                                                                     "Faria Linear",
                                                                     "Faria Power",
                                                                     "Peverell",
                                                                     "Whitty Quad")))) +
  coord_cartesian(xlim =c(400, 6350)) +
  geom_point() +
  scale_color_manual(labels=c('   ','   ','   ','   ','   ', '   '),
                     values = c('darkblue',
                                'orange',
                                'yellow',
                                rgb(0.2,0.4,0.1,0.7),
                                "lightblue")) +
  stat_smooth(aes(x=TLTrue, y=StResS, color=Publication), 
              method = lm, se = FALSE) +
  annotate(geom = "label",
           label = c('YOY', 'Juvenile', 'Sub-Adult', 'Adult'),
           x = c((760+1200)/2, (1200+2700)/2, (2700+3000)/2, (3000+6400)/2),
           y = c(52, 52, 52, 52),
           size = c(4,4,4,4),
           fill = c("#5ab4ac", "#f5f5f5", "lightgrey", "#d8b365"),
           colour = c("white", "black", "black", "white"),
           angle = 0,
           vjust = 1) +
  theme(plot.title = element_text(face = "italic"),
        legend.key = element_rect(fill = "white"),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "bottom",
        legend.spacing.x = unit(1, "cm")) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "A. cuspidata",
       x = bquote("Total Length (mm)"),
       y = bquote("Percent Error (%)"),
       color = "Model")
ALeg

#need legend in between!

#align em
plotsL <- align_plots(BarAR + theme(axis.title.x = element_blank(),
                                    legend.position = "none"),
                      ACDP + theme(plot.title = element_blank(),
                                   legend.position = "none"),
                      align = 'v', axis = 'l')

plotsR <- align_plots(BarZR + theme(axis.title.x = element_blank(),
                                    axis.title.y = element_blank(),
                                    legend.position = "none"),
                      PZDP + theme(plot.title = element_blank(),
                                   axis.title.y = element_blank(),
                                   legend.position = "none"),
                      align = 'v', axis = 'l')

#top row bars
topACP <- plot_grid(plotsL[[1]], 
                    plotsR[[1]], 
                    labels = c('a)', 'c)'),
                    label_size = 13,
                    label_fontface = "plain",
                    rel_widths = c(1, 1.5),
                    align = 'h')

leg1 <- get_legend(ALeg)
leg2 <- get_legend(ZLeg)

#leg in middle
legR <- plot_grid(leg1,
                  NULL,
                  leg2,
                  rel_widths = c(1.1, 0.05, 1.4),
                  align = "h",
                  nrow = 1)

#bottom row dots
botPZP <- plot_grid(plotsL[[2]],
                    plotsR[[2]], 
                    labels = c('b)', 'd)'),
                    label_size = 13,
                    label_fontface = "plain",
                    rel_widths = c(1, 1.5))

cowplot::plot_grid(topACP,
                   #legR,
                   botPZP,
                   rel_heights = c(1,2),
                   align = 'h',
                   ncol = 1)

ggsave(
  "Fig5.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8,
  height = 5,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)


##TRL vs SRL-----
####Means----
library(tidyr)
df2 %>%
  group_by(Species) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(NetRes_S, na.rm = TRUE),
    sd = sd(NetRes_S, na.rm = TRUE)
  )%>%
  ungroup()

df2 %>%
  group_by(Species) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(NetRes_T, na.rm = TRUE),
    sd = sd(NetRes_T, na.rm = TRUE)
  )%>%
  ungroup()

#ttests
df2 %>%  #ttest - calc vs true SRL
  group_by(Species) %>%
  do(tidy(wilcox.test(.$Residuals_SRL,
                      .$Residuals_TRL,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      conf.level = 0.99))) %>%
  ungroup()

df2 %>%  #ttest - calc vs true SRL
  group_by(Species) %>%
  do(tidy(wilcox.test(.$logNetSRL,
                      .$logNetTRL,
                      mu = 0,
                      alt = "greater",
                      paired = TRUE,
                      conf.level = 0.99))) %>%
  ungroup()

df2 %>%  #ttest - calc vs true SRL
  group_by(Species) %>%
  do(tidy(wilcox.test(.$NetRes_S,
                      .$NetRes_T,
                      mu = 0,
                      alt = "less",
                      paired = TRUE,
                      conf.level = 0.99))) %>%
  ungroup()

df2 %>%  #ttest - calc vs true SRL
  group_by(Species) %>%
  do(tidy(wilcox.test(.$PESRL,
                      .$PETRL,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      conf.level = 0.99))) %>%
  ungroup()

df2 %>%  #ttest - calc vs true SRL
  group_by(Species) %>%
  do(tidy(wilcox.test(.$Residuals_SRL,
                      .$Residuals_TRL,
                      mu = 0,
                      alt = "greater",
                      paired = TRUE,
                      conf.level = 0.99))) %>%
  ungroup()

df2 %>%  #ttest - SIGNIFICANT - AC / PC
  group_by(Species) %>%
  do(tidy(wilcox.test(.$SRL_Calc,
                      .$TRL_Calc,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      conf.level = 0.99))) %>%
  ungroup()

df2 %>%  #ttest - SIGNIFICANT - AC / PC
  group_by(Species, Publication) %>%
  do(tidy(wilcox.test(.$SRL_Calc,
                      .$TRL_Calc,
                      mu = 0,
                      alt = "two.sided",
                      exact = FALSE,
                      paired = TRUE,
                      conf.level = 0.99))) %>%
  ungroup()

df2 %>%  #ttest - SIGNIFICANT - AC / PC
  group_by(Species, Maturity) %>%
  do(tidy(wilcox.test(.$SRL_Calc,
                      .$TRL_Calc,
                      mu = 0,
                      alt = "two.sided",
                      exact = FALSE,
                      paired = TRUE,
                      conf.level = 0.99))) %>%
  ungroup()

###PRISTIS Bar----

cowplot::plot_grid(StBarCR + theme(axis.text.x = element_blank(),
                 axis.title.x = element_blank(),
                 axis.title.y = element_blank(),
                 plot.title =  element_blank(),
                 legend.position = "none"),
 StBarCRT + theme(axis.text.x = element_blank(),
                  axis.title.x = element_blank(),
                  axis.text.y = element_blank(),
                  plot.title =  element_blank(),
                  axis.title.y = element_blank(),
                  legend.position = "none"),
 StBarPR1 + theme(axis.text.x = element_blank(),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  plot.title =  element_blank(),
                  legend.position = "none"),
 StBarPRT2 + theme(axis.text.x = element_blank(),
                   axis.title.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.title.y = element_blank(),
                   plot.title =  element_blank(),
                   legend.position = "none"),
 StBarZR + theme(axis.title.y = element_blank(),
                 axis.title.x = element_blank(),
                 plot.title =  element_blank(),
                 legend.position = "none"),
 StBarZRT + theme(axis.text.y = element_blank(),
                  plot.title =  element_blank(),
                  axis.title.y = element_blank(),
                  axis.title.x = element_blank()),
 labels = "AUTO", font.label = list(size = 10, face = "bold"),
 nrow = 3, ncol = 2, align = 'h', axis = 'tlrb',
 rel_widths = c(1.5, 1), rel_heights = c(6,8,7))

#Not aligned!

library(ggplot2)
library(cowplot)

plots <- align_plots(StBarCR + theme(axis.text.x = element_blank(),
                                     axis.title.x = element_blank(),
                                     axis.title.y = element_blank(),
                                     plot.title =  element_blank(),
                                     legend.position = "none"),
                     StBarPR1 + theme(axis.text.x = element_blank(),
                                      axis.title.x = element_blank(),
                                      axis.title.y = element_blank(),
                                      plot.title =  element_blank(),
                                      legend.position = "none"),
                     StBarZR + theme(axis.title.y = element_blank(),
                                     axis.title.x = element_blank(),
                                     plot.title =  element_blank(),
                                     legend.position = "none"),
                     align = 'v', axis = 'l')

#now lets define rows

topAP <- plot_grid(plots[[1]], StBarCRT + theme(axis.text.x = element_blank(),
                                               axis.title.x = element_blank(),
                                               axis.text.y = element_blank(),
                                               plot.title =  element_blank(),
                                               axis.title.y = element_blank(),
                                               legend.position = "none"),
                  labels = c('A', 'B'),
                  label_size = 10,
                  rel_widths = c(1.4, 1))
medAP <- plot_grid(plots[[2]], StBarPRT2 + theme(axis.text.x = element_blank(),
                                                axis.title.x = element_blank(),
                                                axis.text.y = element_blank(),
                                                axis.title.y = element_blank(),
                                                plot.title =  element_blank(),
                                                legend.position = "none"),
                  labels = c('C', 'D'),
                  label_size = 10,
                  rel_widths = c(1.4, 1))

botAP <- plot_grid(plots[[3]], StBarZRT + theme(axis.text.y = element_blank(),
                                               plot.title =  element_blank(),
                                               axis.title.y = element_blank(),
                                               axis.title.x = element_blank()),
                  labels = c('E', 'F'),
                  label_size = 10,
                  rel_widths = c(1.4, 1))

PRIST <- cowplot::plot_grid(topAP, 
                   medAP,
                   botAP,
                   labels = c('','','','','',''), 
                   nrow = 3, 
                   #align = 'h', axis = 'l',
                   rel_heights = c(1.2, 1.4, 1.3))

annotate_figure(PRIST,
                left = text_grob("Publication", rot = 90, size = 10),
                bottom = text_grob("Standardised Net Residuals", size = 10))

PRIST

ggsave(
  "PRISTIS-SvT.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 12,
  height = 7.5,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

##Means----
###Anoxy Pubs----
library(tidyverse)

Ares %>%
  group_by(Age_Class, Publication) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)
  )%>%
  ungroup()

Ares %>%
  group_by(Maturity, Publication) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)
  )%>%
  ungroup()

###Clavata Pubs----
library(tidyverse)
print(Cres %>%
  group_by(Age_Class, Publication) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)
  )%>%
  ungroup(), n = Inf)

Cres %>%
  group_by(Size_Class, Publication) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)
  )%>%
  ungroup()

###Pristis Pubs----
Pres %>%
  group_by(Age_Class, Publication) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)
  )%>%
  ungroup()

Pres %>%
  group_by(Maturity, Publication) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)
  )%>%
  ungroup()

###Zijsron Pubs----
Zres %>%
  group_by(Age_Class, Publication) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)
  )%>%
  ungroup()

Zres %>%
  group_by(Maturity, Publication) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)
  )%>%
  ungroup()

##CRITERIA----
#For SRL vs True - see individual spec section t-tests/wilcox tests

Ares %>%
  group_by(Publication, Maturity) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(NetRes_S, na.rm = TRUE),
    sd = sd(NetRes_S, na.rm = TRUE)
  )%>%
  ungroup()

Cres %>%
  group_by(Publication) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)
  )%>%
  ungroup()
Pres %>%
  group_by(Publication) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)
  )%>%
  ungroup()

Zres %>%
  group_by(Publication) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)
  )%>%
  ungroup()

####Net Overall----
ALLNR <- AllR %>%
  group_by(Species, Publication) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(NetRes_S, na.rm = TRUE),
    sd = sd(NetRes_S, na.rm = TRUE)
  )%>%
  ungroup()
print(ALLNR, n = Inf)

ALLNPE <- AllR %>%
  group_by(Species, Publication) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(NPESRL, na.rm = TRUE),
    sd = sd(NPESRL, na.rm = TRUE)
  )%>%
  ungroup()
print(ALLNPE, n = Inf)

ALLNRT <- AllR %>%
  group_by(Species, Publication) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(NetRes_T, na.rm = TRUE),
    sd = sd(NetRes_T, na.rm = TRUE)
  )%>%
  ungroup()
print(ALLNRT, n = Inf)

####Maturity-Error----
AllR %>%
  group_by(Species, Maturity) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(PESRL, na.rm = TRUE),
    sd = sd(PESRL, na.rm = TRUE)
  )%>%
  ungroup()

ALLPE <- AllR %>%
  group_by(Species, Maturity, Publication) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(NPESRL, na.rm = TRUE),
    sd = sd(NPESRL, na.rm = TRUE)
  )%>%
  ungroup()
print(ALLPE, n = Inf)

ALLPET <- AllR %>%
  group_by(Species, Maturity, Publication) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(PETRL, na.rm = TRUE),
    sd = sd(PETRL, na.rm = TRUE)
  )%>%
  ungroup()
print(ALLPET, n = Inf)

####SizeSplit-Error----

PlotSRL <- ggplot(AllR, aes(x=Age_Class, y=Residuals_SRL, color=Publication)) + 
  scale_x_discrete(limits = c("YOY", "juvenile", "sub-adult", "adult")) +
  geom_boxplot() +
  facet_grid(~Species)

PlotSRL

PlotSt <- ggplot(AllR, aes(x=Age_Class, y=PESRL, color=Publication)) + 
  scale_x_discrete(limits = c("YOY", "juvenile", "sub-adult", "adult")) +
  geom_boxplot() +
  facet_grid(~Species)

PlotSt

PlotTRL <- ggplot(AllR, aes(x=Age_Class, y=Residuals_TRL, color=Publication)) + 
  geom_boxplot() +
  facet_grid(~Species) 

cowplot::plot_grid(PlotSRL, PlotTRL, labels = "AUTO", ncol = 1, align ="v")
  
ggplot(AllR, aes(x=Age_Class, y= mean(Residuals_SRL), group=Publication)) +
  facet_grid(~Species) +
  geom_line(aes(color=Publication))+
  geom_point(aes(color=Publication))

AllR %>%
  group_by(Publication) %>% #means Residuals by group TRL
  dplyr::summarise(
    count = n(),
    mean = mean(Residuals_TRL, na.rm = TRUE),
    sd = sd(Residuals_TRL, na.rm = TRUE)
  ) %>%
  ungroup()

AllR %>%
  group_by(Publication) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(NetResid_TRL, na.rm = TRUE),
    sd = sd(NetResid_TRL, na.rm = TRUE)
  )%>%
  ungroup()

###Model----
AllR %>%
  group_by(Species, Model_Type) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(Residuals_SRL, na.rm = TRUE),
    sd = sd(Residuals_SRL, na.rm = TRUE)
  )%>%
  ungroup()

AllR %>%
  group_by(Species, Model_Type) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(NetRes_S, na.rm = TRUE),
    sd = sd(NetRes_S, na.rm = TRUE)
  )%>%
  ungroup()

AllR$Age_Class=factor(AllR$Age_Class, levels = c("adult", "sub-adult", "juvenile", "YOY"))

PlotType <- ggplot(AllR, aes(x=Age_Class, y=Residuals_SRL, color=Model_Type)) + 
  geom_boxplot() +
  facet_grid(~Species) +
  labs(x = "Size Class", y = "Residuals") +
  theme(strip.text.x = element_text(face = "italic")) +
  scale_x_discrete(labels = c('Adult', 'Sub-adult', 'Juvenile', 'YOY')) +
  guides(color=guide_legend(title="Model Type"))
PlotType

ggsave(
  "PlotType.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 12,
  height = 4,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

kruskal.test(NetRes_S ~ Model_Type, AllR)

Ares %>%
  group_by(ModelT) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)
  )%>%
  ungroup()

Ares %>%
  group_by(ModelT) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(NetRes_S, na.rm = TRUE),
    sd = sd(NetRes_S, na.rm = TRUE)
  )%>%
  ungroup()

summary(aov(StNetResS ~ ModelT, Ares))
kruskal.test(NetRes_S ~ ModelT, Ares)

Cres %>%
  group_by(ModelT) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)
  )%>%
  ungroup()

Cres %>%
  group_by(ModelT) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(NetRes_S, na.rm = TRUE),
    sd = sd(NetRes_S, na.rm = TRUE)
  )%>%
  ungroup()

summary(aov(StNetResS ~ ModelT, Cres))
wilcox.test(NetRes_S ~ModelT, Cres)

Pres %>%
  group_by(ModelT) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)
  )%>%
  ungroup()

Pres %>%
  group_by(ModelT) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(NetRes_S, na.rm = TRUE),
    sd = sd(NetRes_S, na.rm = TRUE)
  )%>%
  ungroup()

wilcox.test(NetRes_S ~ModelT, Pres)
summary(aov(StNetResS ~ ModelT, Pres))

Zres %>%
  group_by(ModelT) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(Residuals_SRL, na.rm = TRUE),
    sd = sd(Residuals_SRL, na.rm = TRUE)
  )%>%
  ungroup()

Zres %>%
  group_by(ModelT) %>% #means Residuals by group SRL
  dplyr::summarise(
    count = n(),
    mean = mean(NetRes_S, na.rm = TRUE),
    sd = sd(NetRes_S, na.rm = TRUE)
  )%>%
  ungroup()

wilcox.test(NetRes_S ~ModelT, Zres)

##Visualisation----

#Results section - table output
library(tidyverse)
library(gtsummary)

pacman::p_load(
  rio,            # import/export
  here,           # file pathways
  flextable,      # make HTML tables 
  officer,        # helper functions for tables
  tidyverse)      # data management, summary, and visualization

###Normality----

df2 %>%
  ggplot(aes(x=PESRL, fill=Publication))+
  facet_wrap(~Species)+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  labs(x="Residuals - SRL")

df2 %>%
  group_by(Species, Publication) %>%
  shapiro_test(PESRL) %>%
  ungroup()%>%
  print(n=26)

#Subset 

Afin <- dfAR %>% select(Publication, ModelT, Maturity, TLTrue, TLCalc_SRL, NetRes_S, StNetResS, StResS)
OrgA <- Afin %>% 
  group_by(Publication) %>%
  summarise(                                                         # only the below summary columns will be returned
    N = sum(t.test(TLTrue, TLCalc_SRL, paired = TRUE, conf.level = 0.95)$parameter,1),
    same_pred_p = round(t.test(TLTrue, TLCalc_SRL, paired = TRUE, conf.level = 0.95)$p.value, digits = 2), #ttest attempt
    same_pred_t = round(t.test(TLTrue, TLCalc_SRL, paired = TRUE, conf.level = 0.95)$statistic, digits = 2),
    net_mean = round(mean(NetRes_S, na.rm = T), digits = 2),     # mean net res
    net_sd = round(sd(NetRes_S, na.rm = T), digits = 2),  # standard deviation net res
    gradient = round(lm(StResS ~ TLTrue)$coefficients[2], digits = 4),
    gradient_se = round(summary(lm(StResS ~ TLTrue))$coefficients[4], digits = 4),
    pe_mean_ju = round(mean(StNetResS[Maturity == "Immature"], na.rm = T), digits = 1),     # mean net res
    pe_sd_ju = round(sd(StNetResS[Maturity == "Immature"], na.rm = T), digits = 1),
    pe_mean_ad = round(mean(StNetResS[Maturity == "Mature"], na.rm = T), digits = 1),     # mean net res
    pe_sd_ad = round(sd(StNetResS[Maturity == "Mature"], na.rm = T), digits = 1),
    model = first(ModelT))  # standard deviation net res
  
flexA <- flextable(OrgA)
flexA %>%
  autofit()
flexA <- flexA %>% 
  set_header_labels(         # Rename the columns in original header row
    Publication = "Model", 
    N = "N",                  
    same_pred_p = "p-val",
    same_pred_t = "t-stat",
    net_mean = "Mean",
    net_sd = "SD",
    gradient = "Gradient",
    gradient_se = "SE",
    pe_mean_ju = "Mean",
    pe_sd_ju = "SD",
    pe_mean_ad = "Mean",
    pe_sd_ad = "SD",
    model = "Model Type")%>% 
  add_header_row(
    top = TRUE,     # New header goes on top of existing header row
    values = c("",     
               "", 
               "1. Calculated = Predicted",   
               "",
               "2. Low Net Residuals (mm)",
               "",         
               "3. Zero Gradient",             
               "",
               "4a. Low Error Juveniles (%)",
               "",
               "4b. Low Error Adults (%)",
               "",
               "")) %>% 
  merge_at(i = 1, j = 3:4, part = "header") %>% # Horizontally merge columns 3 to 4 in new header row
  merge_at(i = 1, j = 5:6, part = "header") %>%  
  merge_at(i = 1, j = 7:8, part = "header") %>%  
  merge_at(i = 1, j = 9:10, part = "header") %>%  
  merge_at(i = 1, j = 11:12, part = "header")     

border_style = officer::fp_border(color="black", width=1)

flexA %>%
flextable::align(align = "center", j = c(3:12), part = "all") %>%
  border_remove() %>%  
  # add horizontal lines via a pre-determined theme setting
  theme_booktabs() %>% 
  # add vertical lines to separate Recovered and Died sections
  vline(part = "all", j = 2, border = border_style) %>%   
  vline(part = "all", j = 4, border = border_style) %>%   
  vline(part = "all", j = 6, border = border_style) %>%   
  vline(part = "all", j = 8, border = border_style) %>%   
  vline(part = "all", j = 12, border = border_style) %>%      
  fontsize(i = 1, size = 11, part = "header") %>%   # adjust font size of header
  bold(i = 1, bold = TRUE, part = "header") %>%     # adjust bold face of header
  bg(part = "body", bg = "gray95")  %>%
  bg(j = 3, i = ~ same_pred_p >= 0.1, part = "body", bg = "palegreen3") %>% 
  bg(j = 5, i = ~ net_mean <= 150, part = "body", bg = "palegreen3") %>% 
  bg(j = 7, i = ~ gradient <= 0.008, part = "body", bg = "palegreen3") %>%
  bg(j = 9, i = ~ pe_mean_ju <= 7, part = "body", bg = "palegreen3") 

flexA <- flextable(OrgA)
flexA %>%
  autofit()
flexA <- flexA %>% 
  set_header_labels(         # Rename the columns in original header row
    Publication = "Model", 
    N = "N",                  
    same_pred_p = "p-val",
    same_pred_t = "t-stat",
    net_mean = "Mean",
    net_sd = "SD",
    gradient = "Gradient",
    gradient_se = "SE",
    pe_mean_ju = "Mean",
    pe_sd_ju = "SD",
    pe_mean_ad = "Mean",
    pe_sd_ad = "SD",
    model = "Model Type")%>% 
  add_header_row(
    top = TRUE,     # New header goes on top of existing header row
    values = c("",     
               "", 
               "1. Calculated = Predicted",   
               "",
               "2. Low Net Residuals (mm)",
               "",         
               "3. Zero Gradient",             
               "",
               "4a. Low Error Juveniles (%)",
               "",
               "4b. Low Error Adults (%)",
               "",
               "")) %>% 
  merge_at(i = 1, j = 3:4, part = "header") %>% # Horizontally merge columns 3 to 4 in new header row
  merge_at(i = 1, j = 5:6, part = "header") %>%  
  merge_at(i = 1, j = 7:8, part = "header") %>%  
  merge_at(i = 1, j = 9:10, part = "header") %>%  
  merge_at(i = 1, j = 11:12, part = "header")     

border_style = officer::fp_border(color="black", width=1)

flexA %>%
flextable::align(align = "center", j = c(3:12), part = "all") %>%
  border_remove() %>%  
  # add horizontal lines via a pre-determined theme setting
  theme_booktabs() %>% 
  # add vertical lines to separate Recovered and Died sections
  vline(part = "all", j = 2, border = border_style) %>%   
  vline(part = "all", j = 4, border = border_style) %>%   
  vline(part = "all", j = 6, border = border_style) %>%   
  vline(part = "all", j = 8, border = border_style) %>%   
  vline(part = "all", j = 12, border = border_style) %>%      
  fontsize(i = 1, size = 11, part = "header") %>%   # adjust font size of header
  bold(i = 1, bold = TRUE, part = "header") %>%     # adjust bold face of header
  bg(part = "body", bg = "gray95")  %>%
  bg(j = 3, i = ~ same_pred_p >= 0.1, part = "body", bg = "palegreen3") %>% 
  bg(j = 5, i = ~ net_mean <= 150, part = "body", bg = "palegreen3") %>% 
  bg(j = 7, i = ~ gradient <= 0.008, part = "body", bg = "palegreen3") %>%
  bg(j = 9, i = ~ pe_mean_ju <= 7, part = "body", bg = "palegreen3") 

##LetsGo ALL----

FinalTab <- df2 %>% select(Species, Publication, Model_Type, Maturity, TLTrue, SRL_Calc, NetRes_S, slogSRL, logNetSRL, NPESRL, PESRL)
OrgAll <- FinalTab %>% 
  group_by(Species, Publication) %>%
  summarise(                                                         # only the below summary columns will be returned
    N = sum(t.test(TLTrue, SRL_Calc, paired = TRUE, conf.level = 0.95)$parameter,1),
    same_pred_p = round(wilcox.test(TLTrue, SRL_Calc, paired = TRUE, conf.level = 0.95)$p.value, digits = 2), #ttest attempt
    same_pred_t = round(wilcox.test(TLTrue, SRL_Calc, paired = TRUE, conf.level = 0.95)$statistic, digits = 2),
    net_mean = round(mean(NetRes_S, na.rm = T), digits = 2),     # mean net res
    net_se = round(sd(NetRes_S, na.rm = T)/sqrt(n()), digits = 2),  # standard deviation net res
    gradient = round(lm(PESRL ~ TLTrue)$coefficients[2], digits = 4),
    gradient_se = round(summary(lm(PESRL ~ TLTrue))$coefficients[4], digits = 4),
    pe_mean_ju = round(mean(NPESRL[Maturity == "Immature"], na.rm = T), digits = 1),     # mean net res
    pe_se_ju = round(sd(NPESRL[Maturity == "Immature"]/sqrt(n()), na.rm = T), digits = 1),
    pe_mean_ad = round(mean(NPESRL[Maturity == "Mature"], na.rm = T), digits = 1),     # mean net res
    pe_se_ad = round(sd(NPESRL[Maturity == "Mature"]/sqrt(n()), na.rm = T), digits = 1),
    model = first(Model_Type))
OrgAll

flexALL <- flextable(OrgAll)
flexALL %>%
  autofit()
flexALL <- flexALL %>% 
  set_header_labels(Species = "",         # Rename the columns in original header row
    Publication = "Model", 
    N = "N",                  
    same_pred_p = "p",
    same_pred_t = "W",
    net_mean = "Mean",
    net_se = "SE",
    gradient = "Gradient",
    gradient_se = "SE",
    pe_mean_ju = "Mean",
    pe_se_ju = "SE",
    pe_mean_ad = "Mean",
    pe_se_ad = "SE",
    model = "Model Type")%>% 
  add_header_row(
    top = TRUE,     # New header goes on top of existing header row
    values = c("",
               "",
               "", 
               "1. Calculated = Predicted",   
               "",
               "2. Low Net Residuals (mm)",
               "",         
               "3. Zero Gradient",             
               "",
               "4a. Low Error Juveniles (%)",
               "",
               "4b. Low Error Adults (%)",
               "",
               "")) %>% 
  colformat_num(j = c(6,7), digits = 1) %>%
  colformat_num(j = 8, digits = 3) %>%
  merge_at(i = 1, j = 4:5, part = "header") %>% # Horizontally merge columns 3 to 4 in new header row
  merge_at(i = 1, j = 6:7, part = "header") %>%  
  merge_at(i = 1, j = 8:9, part = "header") %>%  
  merge_at(i = 1, j = 10:11, part = "header") %>%  
  merge_at(i = 1, j = 12:13, part = "header")     

border_style = officer::fp_border(color="black", width=1)

flexALL <- flexALL %>%
  border_remove() %>%  
  # add horizontal lines via a pre-determined theme setting
  theme_booktabs() %>% 
  # add vertical lines to separate Recovered and Died sections
  flextable::align(align = "center", part = "all") %>%
  merge_v(j = 1, target = NULL, part = "body", combine = FALSE) %>%
  vline(part = "all", j = 3, border = border_style) %>%   
  vline(part = "all", j = 5, border = border_style) %>%   
  vline(part = "all", j = 7, border = border_style) %>%   
  vline(part = "all", j = 9, border = border_style) %>%   
  vline(part = "all", j = 13, border = border_style) %>%      
  fontsize(i = 1, size = 11, part = "header") %>%   # adjust font size of header
  bold(i = 1, bold = TRUE, part = "header") %>%     # adjust bold face of header
  bg(i = 1:5, part = "body", bg = "gray95")  %>%
  bg(i = 6:11, part = "body", bg = "white")  %>%
  bg(i = 12:19, part = "body", bg = "gray95")  %>%
  bg(i = 20:25, part = "body", bg = "white")  %>%
  bg(j = 4:5, i = ~ same_pred_p >= 0.1, part = "body", bg = "palegreen3") %>% 
  bg(j = 6:7, i = ~ net_mean <= 100, part = "body", bg = "palegreen3") %>% 
  bg(j = 8:9, i = ~ abs(gradient) <= 0.002, part = "body", bg = "palegreen3") %>%
  bg(j = 10:11, i = ~ pe_mean_ju <= 6.5, part = "body", bg = "palegreen3") %>%
  bg(j = 12:13, i = ~ pe_mean_ad <= 6.5, part = "body", bg = "palegreen3") %>%
  bg(j = 14, i = 2, part = "body", bg = "palegreen3") %>%
  bg(j = 14, i = 9, part = "body", bg = "palegreen3") %>%
  bg(j = 14, i = 26, part = "body", bg = "palegreen3") %>%
  bg(j = 14, i = ~ pe_mean_ad <= 6, part = "body", bg = "palegreen3") %>%
  italic(j = 1) %>%
  width(j=1, width = 1.2) %>% 
  width(j=2, width = 1.7) %>%
  width(j=4:12, width = 0.5)
flexALL

print(flexALL, preview = "docx") # Word document example
save_as_image(flexALL, path = "bigtable.png")

#Final plots pretty
ggplot(AllR, aes(Age_Class, Residuals_SRL, color=Species)) +
  geom_boxplot() +
  #coord_flip() +
  labs(title = "Residuals by Species", 
       x= bquote(NULL),
       y= bquote("Residuals"))

library(scales)

#Best Pubs AMSA----

A1<-function(){
  plot(Atest$SRL, Atest$TLCorr,
       xlab="Standard Rostrum Length (mm)", 
       ylab="Total Length (mm)",
       xlim = c(50, 800),
       ylim = c(500, 4000),
       xaxp = c(100, 700, 3),
       yaxp = c(500, 3500, 3))
  title("A. cuspidata", font.main=3,
        adj = 0.225, line = -1.5)
  curve(AWhittyL, from = 100, to = 420,
        add=TRUE,
        col='darkblue',
        lwd=2, lty=5)
  curve(AFariaL, from=380, to=750, 
        add=TRUE, 
        col='orange',
        lwd=2, lty=5)
  curve(BootA,
        add=TRUE)
  abline(v = 400, lty = 3)
  abline(h = 2000, lty = 3)
  # legend("bottomright",
  #        legend = c("Faria, 2007 - Linear Model",
  #                   "Lear et al. 2023 - Female Equation",
  #                   "Whitty et al. 2014 - Table 2 Ratio",
  #                   "Whitty et al. 2014 - Power Model"),
  #        lty = c(2, 1, 2, 1),
  #        col =  c('orange', 'darkgreen', 'darkblue', 'lightblue'),
  #        theme(legend.text=element_text(size=12)))
}
A1()

C1<-function(){
  plot(Ctest$SRL, Ctest$TLCorr,
       xlab="", 
       ylab="Total Length (mm)",
       xlim = c(50, 700),
       ylim = c(500, 4200),
       xaxp = c(100, 700, 3),
       yaxp = c(1000, 4000, 3))
  title("P. clavata", font.main=3,
        adj = 0.2, line = -1.5)
  curve(CWhittyP, from=100, to=530, 
        add=TRUE, lwd=2, col='lightblue')
  abline(v = 300, lty = 3)
  abline(h = 1600, lty = 3)
  #text(x = c(800), y = c(1200), 
  #    labels = c('TL = 2.213SRL^1.157'))
  legend("bottom",   # Position
         inset = -.5, # Distance from the margin as a fraction of the plot region
         legend = c(# "Faria, 2007 - Power", 
           "Whitty et al. 2014"),
         lty = c(1),
         col = c('lightblue'),
         theme(legend.text=element_text(size=8)))
}

C1()

P1<-function(){
  plot(Ptest$SRL, Ptest$TLCorr,
       xlab="", 
       ylab="",
       xlim = c(50, 1500),
       ylim = c(700, 6500),
       xaxp = c(200, 1400, 3),
       yaxp = c(500, 6500, 3))
  title("P. pristis", font.main=3,
        adj = 0.2, line = -1.5)
  #curve(PThorF, from=150, to=1450,
  #       add=TRUE, lwd=2, col='magenta')
  curve(PWhittyL, from=150, to=1420, 
        add=TRUE, lwd=2, lty=2, col='darkblue')
  curve(PThorM, from=150, to=1420, 
        add=TRUE, lwd=2, lty=1, col='magenta')
  abline(v = 350, lty = 3)
  abline(h = 1500, lty = 3)
  # legend("bottom",   # Position
  #        inset = -.5, # Distance from the margin as a fraction of the plot region
  #        legend = c("Whitty et al. 2014 - Table 2"),
  #        lty = c(2),
  #        col = c('darkblue'),
  #        theme(legend.text=element_text(size=8)))
}
P1()

Z1<-function(){
  plot(Ztest$SRL, Ztest$TLCorr,
       xlab="", 
       ylab="",
       main="Pristis zijsron", font.main=3,
       xlim = c(50, 1500),
       ylim = c(700, 6000))
  curve(ZLearF, from=175, to=550,
        add=TRUE, lwd=2, col='darkgreen')
  curve(ZFariaL, from = 550, to = 1450,
        add=TRUE, 
        col='orange', lwd=2, lty=2)
  abline(v = 550, lty = 3)
  abline(h = 2250, lty = 3)
  legend("bottom",   # Position
         inset = -.5, # Distance from the margin as a fraction of the plot region
         legend = c("Lear et al. 2023 - Female",
                    "Faria, 2007 - Linear"),
         lty = c(1, 2),
         col =  c('darkgreen', 'orange'),
         theme(legend.text=element_text(size=8)))
}

Z1()

zmodel <-lm(TLCorr ~ SRL, data = Ztest)

ZNew<-function(){
  plot(Ztest$SRL, Ztest$TLCorr,
       xlab="Standard Rostrum Length (mm)", 
       ylab="",
       xlim = c(50, 1500),
       ylim = c(700, 6000),
       xaxp = c(200, 1400, 3),
       yaxp = c(500, 5000, 3))
  title("P. zijsron", font.main=3,
        adj = 0.2, line = -1.5)
  curve(ZBiskisLin, from = 175, to = 1450,
        add=TRUE, 
        col='seagreen', lwd=2, lty=2)
  abline(v = 550, lty = 3)
  abline(h = 2250, lty = 3)
  lines(Ztest$SRL[order(Ztest$SRL)], predict(zmodel, interval = "confidence")[, "lwr"][order(Ztest$SRL)])
  lines(Ztest$SRL[order(Ztest$SRL)], predict(zmodel, interval = "confidence")[, "upr"][order(Ztest$SRL)])
  # text(x = c(1100), y = c(1900), 
  #      labels = c('TL = 3.843 SRL + 101'))
  #curve(CBiskisL, add=TRUE, lwd=2, col='grey')
  #curve(CBiskisP, add=TRUE, lwd=2, col='black')
}

ZNew()

PubsAMSA <- cowplot::plot_grid(A1, 
                               C1, 
                               P1, 
                               ZNew,
                                labels = "", ncol = 2)
PubsAMSA

ggsave(
  "PubsAMSA.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 9,
  height = 8,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

##Fig7----

FinPubs <- cowplot::plot_grid(C1, P1, A1, ZNew,
                               labels = "", ncol = 2)
FinPubs

ggsave(
  "LastFig.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 9,
  height = 8,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

