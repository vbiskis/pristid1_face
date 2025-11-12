# Details ----
#' 04_residuals_pz_app_plots.R
#' Paper: [Fine-tuning established morphometric models...]
#' DOI: 10.1111/csp2.13308
#' Author: Nikki Biskis
#' Date: 2025-11-12
#' Purpose: Check trends with maturity, model type etc.
#' Dependencies: residual function + plotting src code
#' *Changes from original:
#'  Automation
#'  Look at all pubs/tests + a few postHoc tests/graphics
#' -----------

source('04_residuals_2_fxn.R')
source('src_plots_resid.R')

# Run it----
res_PZ <- morpho_tests(dfZR, "P. zijsron")

## Post-Hoc Pub Diffs ----
dfZR %>%  #ttest - calc vs true SRL
  filter(Publication == "Peverell" |
           Publication == "Faria Linear") %>% 
  group_by(Publication, Maturity) %>%
  do(tidy(wilcox.test(.$TLCalc_SRL,
                      .$TLTrue,
                      mu = 0,
                      alt = "greater",
                      paired = TRUE,
                      conf.level = 0.99))) %>%
  ungroup()

dfZR %>%  #ttest - calc vs true SRL
  filter(Publication == "Lear Female" |
           Publication == "Lear Male") %>% 
  group_by(Publication, Maturity) %>%
  do(tidy(wilcox.test(.$TLCalc_SRL,
                      .$TLTrue,
                      mu = 0,
                      alt = "two.sided",
                      paired = TRUE,
                      conf.level = 0.99))) %>%
  ungroup()

## Interaction Plot----
interaction.plot(x.factor     = dfZR$ModelT,
                 trace.factor = dfZR$Maturity,
                 response     = dfZR$StNetResS,
                 fun = mean,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")

# ahhh concerning
# model type switches with age
library(car)
modelZT = lm(StNetResS ~ ModelT + Maturity + ModelT:Maturity, data = dfZR)
Anova(modelZT, type = "II")

modelZTAM = lm(StNetResS ~ ModelT + Age_Class + ModelT:Age_Class, data = dfZR)
Anova(modelZTAM, type = "II")

## Appdx: Residual Plot----

pzmods <- list( #define model details
  list(
    name = "Peverell, 2010", tl_col = "TL_Pev_SRL", 
    resid_col = "ResidP", method = "lm"
  ),
  list(
    name = "Lear 2023 - Female", tl_col = "KLF", 
    resid_col = "ResidKF", method = "lm"
  ),
  list(
    name = "Lear 2023 - Male", tl_col = "KLM", 
    resid_col = "ResidKM", method = "lm"
  ),
  list(
    name = "Faria, 2007 - Linear", tl_col = "TL_FarL_SRL", 
    resid_col = "ResidFL", method = "lm"
  ),
  list(
    name = "Faria, 2007 - Power", tl_col = "TL_FarE_SRL", 
    resid_col = "ResidFE", method = "loess"
  ),
  list(
    name = "Whitty et al. 2014 - Power", tl_col = "TL_WP_SRL", 
    resid_col = "ResidWP", method = "loess"
  ),
  list(
    name = "Whitty et al. 2014 - Ratio", tl_col = "TL_WL_SRL", 
    resid_col = "ResidWL", method = "lm"
  )
)

pz_plots <- resid_plots(dfZT, pzmods, y_max = 6200)

# Access individual plots
Za <- pz_plots[[1]]
Zb <- pz_plots[[2]]
Zc <- pz_plots[[3]]
Zd <- pz_plots[[4]]
Ze <- pz_plots[[5]]
Zf <- pz_plots[[6]]
Zg <- pz_plots[[7]]

ZResGALL <- cowplot::plot_grid(Zd, 
                               Ze + theme(axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank(),
                                          axis.title.y = element_blank() ),
                               Za + theme(axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank(),
                                          axis.title.y = element_blank() ),
                               Zg,
                               Zf+ theme(axis.text.y = element_blank(),
                                         axis.ticks.y = element_blank(),
                                         axis.title.y = element_blank() ),
                               NULL,
                               Zb,
                               Zc + theme(axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank(),
                                          axis.title.y = element_blank() ),
                               labels = c('a','','','b','','','c',''), ncol = 3, align ="v")

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
