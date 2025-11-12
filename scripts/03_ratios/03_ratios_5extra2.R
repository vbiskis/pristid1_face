# Details ----
#' 03_ratios_5extra2.R
#' Paper: [Fine-tuning established morphometric models...]
#' DOI: 10.1111/csp2.13308
#' Author: Nikki Biskis
#' Date: 2025-11-12
#' Purpose: Extra trends not included in paper
#' Output: State graphs
#' *Changes from original:
#'  split into multiple scripts
#' -----------

source('src functions/src_stats_helper.R')

#State tests not sig----
asso_test(c("LJR_SRL", "SRL_TRL", "SRL_TL"), dfA, 
          c("State", "Source"), test = "kw")
asso_test(c("LJR_SRL", "SRL_TRL", "SRL_TL"), dfC, 
          c("State", "Source"), test = "kw")
asso_test(c("LJR_SRL", "SRL_TRL", "SRL_TL"), dfP, 
          c("State", "Source"), test = "kw")
asso_test(c("LJR_SRL", "SRL_TRL", "SRL_TL"), dfZ, 
          c("State", "Source"), test = "kw")

#State graph----
library(ggside)
StateSRL<- ggplot(df1, aes(x=TL_curve, y=SRL_TL, color=State)) + 
  geom_point() +
  # scale_x_continuous(breaks = breaks_fun, limits = c(0, NA)) + 
  facet_wrap(~ Species,
             scales = "free") +
  labs(x= "TL", y = "SRL/TL",
       title= "SRL/TL Ratio by Location") +
  geom_ysidedensity() +
  geom_smooth(method=lm) +
  theme(strip.text.x = element_text(face = "italic"),
        legend.position = "bottom") +
  scale_color_manual(values = 
                       c( "grey", "skyblue","#CC6600", "maroon", "gold1"))
StateSRL

ggsave(
  "StateDiff.png",
  plot = last_plot(),
  device = NULL,
  path = 'ExtraFiles/',
  scale = 1,
  width = 12,
  height = 6,
  units = c("in", "cm", "mm", "px"),
  #dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)
