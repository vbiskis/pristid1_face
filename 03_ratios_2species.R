# Details ----
#' 03_ratios_2species.R
#' Paper: [Fine-tuning established morphometric models...]
#' DOI: 10.1111/csp2.13308
#' Author: Nikki Biskis
#' Date: 2025-11-12
#' Purpose: Test species morpho differences
#' Output: Values for section 3.4.1-4; Figures
#' *Changes from original:
#'  used function to simplify tests
#'  split into multiple scripts
#' -----------

source('src_stats_helper.R')

# Check Data----
## AC----
check_norm(dfA, cols = c("LJR_SRL", "SRL_TRL", "SRL_TL", "TL_curve")) #its mainly LJR that's off
dfA$Age_Class <- as.factor(dfA$Age_Class)
leveneTest(SRL_TL ~ Age_Class, data = dfA) #equal var
leveneTest(SRL_TRL ~ Age_Class, data = dfA) #equal var

asso_test(c("LJR_SRL", "SRL_TRL", "SRL_TL"), dfA, 
          c("TL_Meas", "TL_Est", "Rost_Real", "Multiple_Planes", "TL_Real"), 
          test = "wilcox")

## PC----
check_norm(dfC, cols = c("LJR_SRL", "SRL_TRL", "SRL_TL")) #all normal
dfC$Age_Class <- as.factor(dfC$Age_Class)
leveneTest(SRL_TL ~ Age_Class, data = dfC) #equal var
leveneTest(SRL_TRL ~ Age_Class, data = dfC) #equal var

asso_test(c("LJR_SRL", "SRL_TRL", "SRL_TL"), dfC, 
          c("TL_Meas", "TL_Est", "Rost_Real", "Multiple_Planes", "TL_Real"), 
          test = "wilcox")

## PP----
check_norm(dfP, cols = c("LJR_SRL", "SRL_TRL", "SRL_TL")) #LJR a bit skewed
dfP$Age_Class <- as.factor(dfP$Age_Class)
leveneTest(SRL_TL ~ Age_Class, data = dfP) #equal var
leveneTest(SRL_TRL ~ Age_Class, data = dfP) #equal var

asso_test(c("LJR_SRL", "SRL_TRL", "SRL_TL"), dfP, 
          c("TL_Meas", "TL_Est", "Rost_Real", "Multiple_Planes"), 
          test = "wilcox")

## PZ----
check_norm(dfZ, cols = c("LJR_SRL", "SRL_TRL", "SRL_TL")) #all normal
dfZ$Age_Class <- as.factor(dfZ$Age_Class)
leveneTest(SRL_TL ~ Age_Class, data = dfZ) #equal var
leveneTest(SRL_TRL ~ Age_Class, data = dfZ) #equal var

asso_test(c("LJR_SRL", "SRL_TRL", "SRL_TL"), dfZ, 
          c("TL_Meas", "TL_Est", "Rost_Real", "Multiple_Planes", "TL_Real"), 
          test = "wilcox")

#All passed

# Morpho Trends----
##AC----
asso_test(c("SRL_TRL", "SRL_TL", "TRL_TL"), dfA, "Age_Class")
summary(lm(log(TRL) ~ log(TL_curve), data = dfA)) #isometric
t.test(formula = SRL_TRL ~ Age5, data = dfA) #greatest diff

##PC----
asso_test(c("SRL_TRL", "SRL_TL"), dfC, "Age_Class")
summary(lm(log(SRL) ~ log(TL_stats), data = dfC)) #allometric
t.test(formula = SRL_TL ~ Age3, data = dfC) #greatest diff

##PP----
asso_test(c("SRL_TRL", "SRL_TL"), dfP, "Age_Class")
summary(lm(log(SRL) ~ log(TL_curve), data = dfP)) #allometric
t.test(formula = SRL_TL ~ AgeC4, data = dfP)  #greatest diff

##PZ----
asso_test(c("SRL_TRL", "SRL_TL"), dfZ, "Age_Class")
summary(lm(log(SRL) ~ log(TL_curve), data = dfZ)) #isometric

##Figure 6----
theme_set(theme_classic() +
            theme(axis.text.x = element_text(size = 13),
                  axis.text.y = element_text(size = 13),
                  axis.title.x = element_text(size = 14),
                  axis.title.y = element_text(size = 14),
                  legend.title = element_text(size = 14),
                  legend.text = element_text(face = "italic", size = 13)))

SRLTL <- ggplot(df1, aes(x=TL_curve, 
                         y=SRL_TL, color=Species, shape = Species)) + 
  geom_point() +
  labs(x= "TL (mm)", y = "SRL/TL") +
  geom_smooth(method=lm) +
  theme(legend.position = "none") +
  scale_shape_manual(values = c(4, 15, 16, 17)) +
  scale_color_manual(values = c("blue4","gold2", "red3", "darkgreen")) 

SRLTRL <- ggplot(df1, aes(x=TL_curve, 
                          y=SRL_TRL, color=Species, shape = Species)) + 
  geom_point()+
  labs(x= "TL (mm)", y = "SRL/TRL") +
  geom_smooth(method=lm) +
  theme(legend.position = c(0.75, 0.24)) +
  scale_shape_manual(values = c(4, 15, 16, 17)) +
  scale_color_manual(values = c("blue4","gold2", "red3", "darkgreen")) 

RatiosPair <- cowplot::plot_grid(SRLTL, SRLTRL,
                                 labels = "", ncol = 2)

ggsave(
  "RatiosPost.tiff",
  plot = RatiosPair,
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
