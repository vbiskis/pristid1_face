# Details ----
#' 03_ratios_trends.R
#' Paper: [Fine-tuning established morphometric models...]
#' DOI: 10.1111/csp2.13308
#' Author: Nikki Biskis
#' Date: 2025-11-12
#' Purpose: Pull summary stats, morphometrics vals
#' Output: Values for section 3.2, 3.4; Table S3
#' *Changes from original:
#'  used function to simplify tests
#'  split into multiple scripts
#' -----------

source('src_stats_helper.R')

#Basic Summary Stat----
sumtable(df1, group = "Species", group.long=T)

#Section 3.1----
##Species----
df1 %>% 
  group_by(Species) %>% 
  summarise(
    maxTL = max(TL_curve, na.rm = TRUE),
    minTL = min(TL_curve, na.rm = TRUE),
    count = n()
  ) 

##State----
n_distinct(df1$ID)

df1 %>% 
  group_by(State) %>% 
  summarise(
    count = n(),
    percent = count/2.43
  ) 

#Section 3.4----
check_norm(df1, cols = c("LJR_SRL", #skew right
                         "SRL_TL", #fine
                         "SRL_TRL", #skew left
                         "TRL_TL", #fine
                         "TL_curve") #skew right
           )

##LJR Values----
group_by(df1, Species) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(LJR_SRL, na.rm = TRUE),
    sd = sd(LJR_SRL, na.rm = TRUE)
  ) %>%
  ungroup()

###KW----
kruskal.test(LJR_SRL ~ Species, data = df1)

##SRL/TL----
summary(aov(SRL_TL ~ Species, data = df1))
summary(aov(SRL_TL ~ Species*Maturity, data = df1))

###Appendix S3----
group_by(df1, Species) %>%
  dplyr::summarise(
    mean = mean(SRL_TL, na.rm = TRUE),
    sd = sd(SRL_TL, na.rm = TRUE),    
    count = n(),
    minTL = min(TL_curve, na.rm = TRUE),
    maxTL = max(TL_curve, na.rm = TRUE)  
  ) %>%
  ungroup()

##SRL/TRL----
summary(aov(SRL_TRL ~ Species, data = df1))
summary(aov(SRL_TRL ~ Species*Maturity, data = df1))

###Appendix S3----
group_by(df1, Species) %>%
  summarise(
    mean = mean(SRL_TRL, na.rm = TRUE),
    sd = sd(SRL_TRL, na.rm = TRUE),
    count = n(),
    minTL = min(TL_curve, na.rm = TRUE),
    maxTL = max(TL_curve, na.rm = TRUE)  
  ) %>%
  ungroup()

