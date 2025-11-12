# Details ----
#' 05_residuals_ac_app_plots.R
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

all_morpho <- readRDS("morphodat.rds")
list2env(all_morpho, envir = .GlobalEnv) #hell ya

source('src functions/src_morpho_tests.R')
source('src functions/src_plots_resid.R')

# Run it----
res_AC <- morpho_tests(dfAR, "A. cuspidata")

## Interaction Plot----
interaction.plot(x.factor     = dfAR$ModelT,
                 trace.factor = dfAR$Maturity,
                 response     = dfAR$StNetResS,
                 fun = mean,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")

# no surprises there -> linear > power > quad
# work best for juveniles

dfAR %>%
  group_by(ModelT, Maturity) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)) %>%
  ungroup()

library(car)
modelAT = lm(StNetResS ~ ModelT + Maturity + ModelT:Maturity, data = dfAR)
Anova(modelAT, type = "II")

## Appdx: Residual Plot----
acmods <- list( #define ac model details
  list(
    name = "Peverell, 2010", tl_col = "TL_Pev", 
    resid_col = "ResidP", method = "lm"
  ),
  list(
    name = "Faria, 2007 - Linear", tl_col = "TL_FarL", 
    resid_col = "ResidFL", method = "lm"
  ),
  list(
    name = "Faria, 2007 - Power", tl_col = "TL_FarE", 
    resid_col = "ResidFE", method = "loess"
  ),
  list(
    name = "Whitty et al. 2014 - Quadratic", tl_col = "TL_WQ", 
    resid_col = "ResidWQ", method = "loess"
  ),
  list(
    name = "Whitty et al. 2014 - Ratio", tl_col = "TL_WL", 
    resid_col = "ResidWL", method = "lm"
  )
)

ac_plots <-resid_plots(dfAT, acmods)

# Access individual plots
Aa <- ac_plots[[1]]
Ab <- ac_plots[[2]]
Ac <- ac_plots[[3]]
Ad <- ac_plots[[4]]
Ae <- ac_plots[[5]]

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
                               labels = "auto", ncol = 3, align ="v")

ggsave(
  "App_ACRes.png",
  plot = AResGALL,
  device = NULL,
  path = 'figs/residual plots/',
  scale = 1,
  width = 10,
  height = 5,
  units = c("in", "cm", "mm", "px"),
  #dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)
