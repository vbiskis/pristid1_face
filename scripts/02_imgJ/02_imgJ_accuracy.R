# Details ----
#' 02_init_data_check.R
#' Paper: [Fine-tuning established morphometric models...]
#' DOI: 10.1111/csp2.13308
#' Author: Nikki Biskis
#' Date: 2025-11-11
#' Purpose: Checks for normality/general trends
#' Output: N/A
#' 
#' *Changes from original:
#'  wrote function for normality (commonly reproduced)
#'  turned into RMarkdown (?) as look at data structure
#' -----------

source('src functions/src_stats_helper.R')
library(vtable)
library(ggplot2)

#Quick Look----
sumtable(dfI, group = "Species", group.long=T)

#Error Analysis----
##Normality----
check_norm(dfI, cols = c("ErrTL", #looks good
                         "ErrTTC", #great
                         "ErrTRL", #fine
                         "ErrSRL", #okay
                         "ErrSRW")) #fine

##Means----
dfI2 %>%
  ggplot(aes(x=Error, fill=Meas))+
  geom_density(alpha=0.5)+
  scale_x_log10() +
  labs(x="Percent Error") #seeing range of percent error

Err1 <- dfI2 %>%
  group_by(Meas) %>%
  summarise(
    count = n(),
    mean = mean(NE, na.rm = TRUE),
    sd = sd(NE, na.rm = TRUE)
  ) %>%
  ungroup()
print(Err1, n = Inf) #widths not reliable, but lengths look good!

#For Paper----
##Report n----
#using I1 - no widths (TW, SRW)
Err2 <- dfI1 %>%
  group_by(Meas) %>%
  summarise(
    count = n(),
    mean = mean(NE, na.rm = TRUE),
    sd = sd(NE, na.rm = TRUE)
  ) %>%
  ungroup()
print(Err2, n = Inf) # all measures using

dfI1 %>% 
  group_by(Species) %>% 
  summarise(
    count = n_distinct(ID)
  )

##Paired tests----
pair_test(dfI, "M_SRL", "J_SRL", test = "t", paired = TRUE) #not sig
pair_test(dfI, "M_TRL", "J_TRL", test = "t", paired = TRUE) #not sig
pair_test(dfI, "M_TL", "J_TL", test = "t", paired = TRUE) #not sig
pair_test(dfI, "M_TTC", "J.TTC", test = "t", paired = TRUE) #not sig

pair_test(dfI, "NETRL", "NESRL", test = "wilcox", paired = FALSE,
          group_by = "Species") #not sig

##Associations----
asso_test(c("ErrSRL", "ErrTRL", "ErrTL", "ErrSRW"), dfI, "Scale", "t")
asso_test(c("ErrSRL", "ErrTRL", "ErrTL", "ErrSRW",
            "NESRL", "NETRL", "NETL"), dfI, c("Species", "Type", "Researcher")) #default separates
asso_test(c("ErrSRL", "ErrTRL", "ErrTL", "ErrSRW",
            "NESRL", "NETRL", "NETL"), dfI, c("Species", "Type", "Species:Type"), separate = FALSE)

asso_test("NE", dfI1, c("Species", "Scale", "Meas"))
asso_test("NE", dfI1, c("Meas", "Species", "Meas:Species"))

#Final Err Vals----
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

##Extra Trends----
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
  group_by(Species) %>%
  summarise(
    count = n(),
    mean = mean(NE, na.rm = TRUE),
    sd = sd(NE, na.rm = TRUE)
  ) %>%
  ungroup()
print(Err4, n = Inf)

#Plot----
ggplot(dfI2, aes(x=factor(Meas,
                        level = c('TL', 'TTC', 'TRL', 'SRL', 'SRW', 'TW')),
               y=Error)) + 
  geom_boxplot() +
  facet_grid(~ Species) +
  theme(legend.text = element_text(face = "italic")) +
  theme(axis.text = element_text(face = "italic")) +
  labs(x = 'Measurement',
       y = "Percent Error")

