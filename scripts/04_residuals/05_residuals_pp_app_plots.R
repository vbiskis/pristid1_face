# Details ----
#' 05_residuals_pp_app_plots.R
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
res_PP <- morpho_tests(dfPR, "P. pristis")

## Post-Hoc Pub Diffs ----
t.test(dfPT$SNStFL, dfPT$SNStFP,
       alt = "two.sided", conf.level = 0.95)

t.test(dfPT$SNStFL, dfPT$SNStWL,
       alt = "two.sided", conf.level = 0.95) #slightly less err in Whitty, not sig

t.test(dfPT$SNStP, dfPT$SNStWL,
       alt = "two.sided", conf.level = 0.95) #sig less err in WL than Pev 

## Model Type ----
interaction.plot(x.factor     = dfPR$ModelT,
                 trace.factor = dfPR$Maturity,
                 response     = dfPR$StNetResS,
                 fun = mean,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")

# linear much better
# juvenile prefers power but, then over est adults

dfPR %>%
  group_by(ModelT, Maturity) %>% #means Residuals by group Pub
  dplyr::summarise(
    count = n(),
    mean = mean(StNetResS, na.rm = TRUE),
    sd = sd(StNetResS, na.rm = TRUE)) %>%
  ungroup()

library(car)
modelPT = lm(StNetResS ~ ModelT + Maturity + ModelT:Maturity, data = dfPR)
Anova(modelPT, type = "II")

## Appdx: Residual Plot----
ppmods <- list( #define model details
  list(
    name = "Peverell, 2010", tl_col = "Pev_SRL", 
    resid_col = "ResidP", method = "lm"
  ),
  list(
    name = "Morgan, 2011", tl_col = "Morg_SRL", 
    resid_col = "ResidM", method = "lm"
  ),
  list(
    name = "Faria, 2007 - Linear", tl_col = "FariaL_SRL", 
    resid_col = "ResidFL", method = "lm"
  ),
  list(
    name = "Faria, 2007 - Power", tl_col = "FariaE_SRL", 
    resid_col = "ResidFE", method = "loess"
  ),
  list(
    name = "Thorburn, 2007 - Male", tl_col = "ThorM_SRL", 
    resid_col = "ResidThorM", method = "loess"
  ),
  list(
    name = "Thorburn, 2007 - Female", tl_col = "ThorF_SRL", 
    resid_col = "ResidThorF", method = "loess"
  ),
  list(
    name = "Whitty et al. 2014 - Power", tl_col = "WhittyP_SRL", 
    resid_col = "ResidWC", method = "loess"
  ),
  list(
    name = "Whitty et al. 2014 - Ratio", tl_col = "WhittyL_SRL", 
    resid_col = "ResidWL", method = "lm"
  )
)

pp_plots <- resid_plots(dfPT, ppmods, y_max = 7500)
Pa <- pp_plots[[1]]
Pb <- pp_plots[[2]]
Pc <- pp_plots[[3]]
Pd <- pp_plots[[4]]
Pe <- pp_plots[[5]]
Pf <- pp_plots[[6]]
Pg <- pp_plots[[7]]
Ph <- pp_plots[[8]]

PResGALL <- cowplot::plot_grid(Pc, 
                               Ph + theme(axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank(),
                                          axis.title.y = element_blank() ),
                               Pa + theme(axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank(),
                                          axis.title.y = element_blank() ),
                               Pb + theme(axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank(),
                                          axis.title.y = element_blank() ),
                               Pd,
                               Pg + theme(axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank(),
                                          axis.title.y = element_blank() ), 
                               Pe + theme(axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank(),
                                          axis.title.y = element_blank() ),
                               Pf + theme(axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank(),
                                          axis.title.y = element_blank() ),
                               labels = "auto", ncol = 4, align ="v")

ggsave(
  "App_PPRes.png",
  plot = PResGALL,
  device = NULL,
  path = 'figs/residual plots/',
  scale = 1,
  width = 12.5,
  height = 5,
  units = c("in", "cm", "mm", "px"),
  #dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)
